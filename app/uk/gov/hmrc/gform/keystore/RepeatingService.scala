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

  def getRepeatingGroup(id: FormComponentId, formTemplate: FormTemplate, shape: Shape): RepeatingStructure =
    formTemplate
      .sections
      .flatMap(_.fields)
      .find(_.id == id)
      .map(maybeRepeatGroup(_, shape)).getOrElse(Nil)

  def getRepeatingGroupForSummary(id: FormComponentId, formTemplate: FormTemplate, shape: Shape): RepeatingStructure =
    formTemplate
      .sections
      .flatMap(_.fields)
      .find(_.id == id)
      .map(maybeRepeatGroupForSummary(_, shape)).getOrElse(Nil)

  def getAllRepeatingGroups(shape: Shape, formTemplate: FormTemplate)(implicit ex: ExecutionContext): Future[CacheMap] =
    CacheMap(
      "Generated",
      shape
        .groups
        .keys
        .map(id => id -> findGroup(formTemplate, id).map(x => maybeRepeatGroup(x, shape)).fold(Json.toJson(RepeatingGroup(List.empty[List[FormComponent]], false)))(x => Json.toJson(RepeatingGroup(x, true)))).toMap
    ).pure[Future]

  def getAllSections(shape: Shape, formTemplate: FormTemplate, data: Map[FormComponentId, Seq[String]])(implicit ex: ExecutionContext): Future[List[Section]] = {

    val x: List[Future[Section]] = for {
      s <- formTemplate.sections
      ss <- maybeRepeatSection(s, shape, formTemplate, data)
    } yield ss
    val xx: Future[List[Section]] = Future.sequence(x)
    xx

  }

  private def extractFieldId(expr: TextExpression) = {
    expr.expr match {
      case FormCtx(fieldId) => fieldId
      case _ => ""
    }
  }
  private def findRepeatingGroupsContainingField(expr: TextExpression, formTemplate: FormTemplate): Set[FormComponent] = {

    val id = extractFieldId(expr)

    def findRepeatingGroups(groupField: Option[FormComponent], fieldList: List[FormComponent]): Set[FormComponent] = {
      fieldList.flatMap { field =>
        field.`type` match {
          case Group(fields, _, repMax, _, _, _) if repMax.isDefined => findRepeatingGroups(Some(field), fields)
          case othertype if groupField.isDefined && field.id.value.equals(id) => List(groupField.get)
          case _ => Nil
        }
      }.toSet
    }

    formTemplate.sections.flatMap(section => findRepeatingGroups(None, section.fields)).toSet
  }

  private def sumFunctionality(expr1: String, shape: Shape, formTemplate: FormTemplate, data: Map[FormComponentId, Seq[String]])(implicit ex: ExecutionContext) = {
    val dataGetter: FormComponentId => Int = fieldId => Try(data.get(fieldId).toList.flatten.headOption.getOrElse("0").toInt).getOrElse(0)
    val cacheMap: Future[CacheMap] = getAllRepeatingGroups(shape, formTemplate)
    val repeatingSections: Future[List[List[List[FormComponent]]]] = Future.sequence(formTemplate.sections.flatMap(_.fields).map(fv => (fv.id, fv.`type`)).collect {
      case (fieldId, group: Group) => cacheMap.map(_.getEntry[RepeatingGroup](fieldId.value).map(_.list).getOrElse(Nil))
    })
    Group.getGroup(repeatingSections, FormComponentId(expr1)).flatMap(x => Future.successful(x.map(dataGetter).sum))
  }

  private def getFormFieldIntValue(expr: TextExpression, data: Map[FormComponentId, Seq[String]]): Int = {

    val id = extractFieldId(expr)

    data.get(FormComponentId(id)) match {
      case Some(value) => Try(value.head.toInt) match {
        case Success(intValue) => intValue
        case _ => 0
      }
      case None => 0
    }
  }

  //This Evaluation is for the repeating sections, this will not become values.
  private def evaluateExpression(expr: Expr, shape: Shape, formTemplate: FormTemplate, data: Map[FormComponentId, Seq[String]])(implicit ex: ExecutionContext): Future[Int] = {
    expr match {
      case Add(expr1, expr2) =>
        for {
          first <- evaluateExpression(expr1, shape, formTemplate, data)
          second <- evaluateExpression(expr2, shape, formTemplate, data)
        } yield first + second
      case Multiply(expr1, expr2) =>
        for {
          first <- evaluateExpression(expr1, shape, formTemplate, data)
          second <- evaluateExpression(expr2, shape, formTemplate, data)
        } yield first * second
      case Subtraction(expr1, expr2) =>
        for {
          first <- evaluateExpression(expr1, shape, formTemplate, data)
          second <- evaluateExpression(expr2, shape, formTemplate, data)
        } yield first - second
      case Sum(FormCtx(expr1)) => sumFunctionality(expr1, shape, formTemplate, data)
      case formExpr @ FormCtx(_) => Future.successful(getFormFieldIntValue(TextExpression(formExpr), data))
      case Constant(value) => Try(value.toInt) match {
        case Success(intValue) => Future.successful(intValue)
        case _ => Future.successful(0)
      }
      //      case AuthCtx(value: AuthInfo) =>
      //      case EeittCtx(value: Eeitt) =>
      case _ => Future.successful(0)
    }
  }

  private def getRequestedCount(expr: TextExpression, shape: Shape, formTemplate: FormTemplate, data: Map[FormComponentId, Seq[String]])(implicit ec: ExecutionContext): Future[Int] = {

    val repeatingGroupsFound = findRepeatingGroupsContainingField(expr, formTemplate)

    if (repeatingGroupsFound.isEmpty) {
      evaluateExpression(expr.expr, shape, formTemplate, data)
    } else {
      val groupFieldValue = repeatingGroupsFound.head
      val fieldsInGroup = cacheMap.getEntry[RepeatingGroup](groupFieldValue.id.value).map(_.list).getOrElse(Nil).flatten
      Future.successful(fieldsInGroup.size) //TODO ask tenoch about this case
    }
  }

  private def maybeRepeatSection(section: Section, shape: Shape, formTemplate: FormTemplate, data: Map[FormComponentId, Seq[String]])(implicit ec: ExecutionContext): Future[Seq[Section]] =
    section
      .repeatsMax
      .fold(List(section))(expr => {
        generateDynamicSections(expr, shape, section, formTemplate, data)
      })

  private def generateDynamicSections(repeatsMax: TextExpression, shape: Shape, section: Section, formTemplate: FormTemplate, data: Map[FormComponentId, Seq[String]])(ec: ExecutionContext): Future[List[Section]] = {
    val countF = getRequestedCount(repeatsMax, shape, formTemplate, data)

    for {
      count <- countF
    } yield {
      (1 to count).map { i =>
        ShapeHelper.copySection(section)(i)
      }.toList
    }
  }

  def getSectionFields(section: BaseSection, shape: Shape): List[FormComponent] =
    section
      .fields
      .flatMap(maybeRepeatGroup(_, shape).flatten)

  def getAllFields(shape: Shape, formTemplate: FormTemplate): List[FormComponent] =
    formTemplate
      .sections
      .flatMap(_.fields)
      .flatMap(maybeRepeatGroup(_, shape).flatten) //Gets every field in section.

  private def maybeRepeatGroup(formComponent: FormComponent, shape: Shape): List[List[FormComponent]] = {
    val fs = formComponent match {
      case f @ FormComponent(_, group: Group, _, _, _, _, _, _, _, _, _) =>
        Some(group.repeatsMin.getOrElse(1))
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
    Logger.debug(s"maybeRepeatGroup: ${shape} ${fs}")
    fs
  }

  private def maybeRepeatGroupForSummary(formComponent: FormComponent, shape: Shape): List[List[FormComponent]] = {
    val fs = formComponent match {
      case f @ FormComponent(_, group: Group, _, _, _, _, _, _, _, _, _) =>
        Some(group.repeatsMin.getOrElse(1))
          .fold(List(group.fields))(min =>
            repeat(
              shape
                .groups
                .getOrElse(f.id.value, min),
              ShapeHelper
                .repeatGroupForSummary(group, shape)
            ))
      case _ => List(List(formComponent))
    }
    Logger.debug(s"maybeRepeatGroupForSummary: ${shape} ${fs}")
    fs
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

  private def repeat[A](idx: Int, f: Int => A): List[A] = {
    val x = 1
    val xx = x
    (1 to idx).map(f).toList
  }

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

  def repeatGroupForSummary(group: Group, shape: Shape)(idx: Int): List[FormComponent] =
    if (idx == 1)
      group.fields.map(copyFormComponentForSummary(_, idx - 1))
    else
      group.fields.map(copyFormComponent(_, idx - 1))

  private def copyFormComponent(formComponent: FormComponent, idx: Int): FormComponent =
    formComponent.copy(
      id = rename(idx, formComponent.id),
      label = LabelHelper.buildRepeatingLabel(formComponent, idx + 1),
      shortName = LabelHelper.buildRepeatingLabel(formComponent.shortName, idx + 1)
    )

  private def copyFormComponentForSummary(formComponent: FormComponent, idx: Int): FormComponent =
    formComponent.copy(
      label = LabelHelper.buildRepeatingLabel(formComponent, idx + 1),
      shortName = LabelHelper.buildRepeatingLabel(formComponent.shortName, idx + 1)
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
