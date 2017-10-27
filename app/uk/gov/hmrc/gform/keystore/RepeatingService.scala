/*
 * Copyright 2017 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.gform.keystore

import javax.lang.model.`type`.ExecutableType

import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import cats.data._
import cats.implicits._
import play.api.Logger
import play.api.libs.json.Json
import uk.gov.hmrc.gform.service.LabelHelper
import uk.gov.hmrc.gform.sharedmodel.Shape
import uk.gov.hmrc.gform.sharedmodel.form.RepeatingGroup
import uk.gov.hmrc.http.cache.client.CacheMap

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }

object RepeatingService {

  //TODO once used properly we will need to remove .some.pure[Future] as this is not needed only used to conform with RepeatingComponentService
  type RepeatingStructure = List[List[FormComponent]]

  def getAllSections(formTemplate: FormTemplate, data: Map[FormComponentId, Seq[String]])(implicit ex: ExecutionContext): Future[List[Section]] =
    formTemplate.sections.flatMap(maybeRepeatSection(_, data)).pure[Future]

  private def maybeRepeatSection(section: Section, data: Map[FormComponentId, Seq[String]]): Seq[Section] =
    section
      .repeatsMax
      .fold(List(section))(expr => repeat(ExpressionEvaluator.evalute(expr.expr, data), ShapeHelper.copySection(section)))

  def getRepeatingGroup(id: FormComponentId, formTemplate: FormTemplate, shape: Shape): RepeatingStructure =
    formTemplate
      .sections
      .flatMap(_.fields)
      .find(_.id == id)
      .map(maybeRepeatGroup(_, shape)).getOrElse(Nil)

  def getAllRepeatingGroups(shape: Shape, formTemplate: FormTemplate)(implicit ex: ExecutionContext): Future[CacheMap] =
    CacheMap(
      "Generated",
      shape
        .groups
        .keys
        .map(id => id -> findGroup(formTemplate, id).map(x => maybeRepeatGroup(x, shape)).fold(Json.toJson(RepeatingGroup(List.empty[List[FormComponent]], false)))(x => Json.toJson(RepeatingGroup(x, true)))).toMap
    ).pure[Future]

  def getSectionFields(section: BaseSection, shape: Shape): List[FormComponent] =
    section
      .fields
      .flatMap(maybeRepeatGroup(_, shape).flatten)

  def getAllFields(shape: Shape, formTemplate: FormTemplate): List[FormComponent] =
    formTemplate
      .sections
      .flatMap(_.fields)
      .flatMap(maybeRepeatGroup(_, shape).flatten) //Gets every field in section.

  private def maybeRepeatGroup(formComponent: FormComponent, shape: Shape): List[List[FormComponent]] =
    formComponent match {
      case f @ FormComponent(_, group: Group, _, _, _, _, _, _, _, _, _) =>
        group
          .repeatsMin
          .fold(List(group.fields))(min =>
            repeat(
              shape
                .groups
                .getOrElse(f.id.value, min),
              ShapeHelper
                .repeatGroup(group, shape)
            ))
      case _ => List(List(formComponent))
    }

  def addGroup(shape: Shape, id: FormComponentId, formTemplate: FormTemplate)(implicit ex: ExecutionContext): (Shape, Future[Option[RepeatingStructure]]) =
    findGroup(formTemplate, id.value)
      .collect {
        case x @ FormComponent(_, group: Group, _, _, _, _, _, _, _, _, _) => group
      }.fold(shape -> List.empty[List[FormComponent]].some.pure[Future])(maybeShapeMax(_, shape, id, formTemplate))

  def removeGroup(shape: Shape, id: FormComponentId, data: Map[FormComponentId, Seq[String]], formTemplate: FormTemplate, remove: Int)(implicit ex: ExecutionContext): (Shape, Future[Map[FormComponentId, Seq[String]]]) = {
    val newDataValue = findGroup(formTemplate, id.value)
      .collect {
        case x @ FormComponent(_, group: Group, _, _, _, _, _, _, _, _, _) => group
      }
      .fold(data)(y => newData(shape, id, data, ShapeHelper.repeatGroup(y, shape)(remove), remove, getRepeatingGroup(id, formTemplate, shape)))
    Logger.debug(s"removeGroup data ${newDataValue}")
    getGroup(shape, id, x => Shape((shape.groups - id.value) + (id.value -> x.-(1)), shape.sections)) -> newDataValue.pure[Future]
  }

  private def newData(shape: Shape, formComponentId: FormComponentId, data: Map[FormComponentId, Seq[String]], list: List[FormComponent], remove: Int, grouplist: List[List[FormComponent]]): Map[FormComponentId, Seq[String]] = {
    def renameData(data: Map[FormComponentId, Seq[String]], id: FormComponentId, shape: Shape, newId: FormComponentId) =
      data.get(id).fold(Map.empty[FormComponentId, Seq[String]])(x => Map(newId -> x))
    val (_, clean) = data.partition { case (id, value) => list.map(_.id).contains(id) }
    val (x, oldData) = clean.partition { case (id, value) => grouplist.flatMap(_.map(_.id)).contains(id) }
    Logger.debug(s"newData clean ${clean}")
    Logger.debug(s"newData use ${x}")
    val dataNew =
      grouplist.map(_.map(_.id))
        .filter(y => y.map(z => x.get(z)).forall(_.isDefined))
        .zip(Stream from 1)
        .flatMap {
          case (id, idx) => id.map(y => renameData(x, y, shape, ShapeHelper.rename2(idx, y)))
        }.flatMap(_.toMap)
    oldData ++ dataNew
  }

  private def maybeShapeMax(group: Group, shape: Shape, id: FormComponentId, formTemplate: FormTemplate)(implicit ex: ExecutionContext): (Shape, Future[Option[RepeatingStructure]]) = {
    if (isMax(group.repeatsMax, shape.groups.get(id.value))) {
      shape -> getRepeatingGroup(id, formTemplate, shape).some.pure[Future]
    } else {
      val newShape = getGroup(shape, id, x => Shape((shape.groups - id.value) + (id.value -> x.+(1)), shape.sections))
      newShape -> getRepeatingGroup(id, formTemplate, newShape).some.pure[Future]
    }
  }

  private def findGroup(formTemplate: FormTemplate, id: String): Option[FormComponent] =
    formTemplate
      .sections
      .flatMap(_.fields)
      .find(_.id.value == id)

  private def getGroup(shape: Shape, id: FormComponentId, f: Int => Shape): Shape =
    shape
      .groups
      .get(id.value) //TODO what to do on failure case.
      .fold(shape)(f)

  private def isMax(repeatMax: Option[Int], currentValue: Option[Int]) =
    repeatMax
      .flatMap(
        max =>
          currentValue
            .map(current => current == max)
      )
      .getOrElse(true) //TODO perhaps use eval monad, instead of having this to handle double option.

  private def repeat[A](idx: Int, f: Int => A): List[A] =
    (1 to idx).map(f).toList

}

object ShapeHelper {

  def copySection(section: Section)(idx: Int): Section = {
    def copyField(field: FormComponent): FormComponent =
      field.`type` match {
        case grp @ Group(fields, _, _, _, _, _) =>
          field.copy(
            id = FormComponentId(s"${idx}_${field.id.value}"),
            `type` = grp.copy(fields = fields.map(copyField))
          )
        case _ => field.copy(id = FormComponentId(s"${idx}_${field.id.value}"))
      }
    section.copy(
      title = s"${section.title}_$idx",
      shortName = section.shortName.map(str => s"${str}_$idx"),
      fields = section.fields.map(copyField)
    )
  }

  def repeatGroup(group: Group, shape: Shape)(idx: Int): List[FormComponent] = {
    if (idx == 1)
      group.fields
    else
      group.fields.map(copyFormComponent(_, idx - 1))
  }

  private def copyFormComponent(formComponent: FormComponent, idx: Int): FormComponent =
    formComponent.copy(
      id = rename(idx, formComponent.id),
      label = LabelHelper.buildRepeatingLabel(formComponent, idx),
      shortName = LabelHelper.buildRepeatingLabel(formComponent.shortName, idx)
    )

  def rename(int: Int, formComponentId: FormComponentId) = {
    val z = formComponentId.value.indexOf("_")
    val a = formComponentId.value.drop(z)
    FormComponentId(s"${int}_$a")
  }

  def rename2(int: Int, formComponentId: FormComponentId) = {
    val z = formComponentId.value.indexOf("_")
    val a = formComponentId.value.drop(z + 1)
    if (int == 1)
      FormComponentId(a)
    else
      FormComponentId(s"${int - 1}_$a")
  }

}

object ExpressionEvaluator {

  def evalute(expr: Expr, data: Map[FormComponentId, Seq[String]]): Int = expr match {
    case Add(expr1, expr2) => evalute(expr1, data) + evalute(expr2, data)
    case Multiply(expr1, expr2) => evalute(expr1, data) * evalute(expr2, data)
    case Subtraction(expr1, expr2) => evalute(expr1, data) - evalute(expr2, data)
    case Sum(FormCtx(expr1)) => sum(expr1, data)
    case formExpr @ FormCtx(_) => data.get(formExpr.toFieldId).toList.flatten.headOption.map(toInt).getOrElse(0)
    case Constant(value) => toInt(value)
    case _ => 0
  }

  private def sum(str: String, data: Map[FormComponentId, Seq[String]]): Int = {
    data.filterKeys(_.value.contains(str)).flatMap(_._2.map(toInt)).sum
  }

  private def toInt(string: String) = Try(string.toInt) match {
    case Success(value) => value
    case Failure(_) => 0
  }
}
