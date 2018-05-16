/*
 * Copyright 2018 HM Revenue & Customs
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

import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.service.LabelHelper
import uk.gov.hmrc.gform.sharedmodel.form.{ RepeatingGroup, RepeatingGroupStructure }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.cache.client.CacheMap

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{ Success, Try }
import uk.gov.hmrc.http.HeaderCarrier

class RepeatingComponentService(
  sessionCache: SessionCacheConnector,
  configModule: ConfigModule
) {

  def isRepeating(formTemplate: FormTemplate): Boolean =
    formTemplate.sections.exists { s =>
      isRepeatingSection(s) || hasRepeatingGroup(s.fields)
    }

  private def isRepeatingSection(section: Section): Boolean = section.repeatsMax.isDefined

  private def hasRepeatingGroup(fields: Seq[FormComponent]): Boolean =
    fields.exists(f =>
      f.`type` match {
        case grp @ Group(flds, _, _, _, _, _) =>
          isRepeatingGroup(grp) || hasRepeatingGroup(flds)
        case _ => false
    })

  private def isRepeatingGroup(group: Group): Boolean =
    group.repeatsMax.isDefined

  def fetchSessionCache(formTemplate: FormTemplate)(implicit hc: HeaderCarrier, ec: ExecutionContext) =
    if (isRepeating(formTemplate: FormTemplate))
      sessionCache.fetch
    else
      Future.successful(None)

  def getAllSections(
    formTemplate: FormTemplate,
    data: Map[FormComponentId, Seq[String]],
    sessionCacheMap: Option[CacheMap])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[List[Section]] =
    Future
      .sequence(formTemplate.sections.map { section =>
        {
          if (isRepeatingSection(section)) {
            generateDynamicSections(
              section,
              formTemplate,
              data,
              getAllRepeatingGroups(sessionCacheMap),
              sessionCacheMap)
          } else {
            Future.successful(List(section))
          }
        }
      })
      .map(_.flatten)

  def getAllRepeatingGroups(sessionCacheMap: Option[CacheMap]): CacheMap =
    sessionCacheMap.getOrElse(CacheMap("Empty", Map.empty))

  private def generateDynamicSections(
    section: Section,
    formTemplate: FormTemplate,
    data: Map[FormComponentId, Seq[String]],
    cacheMap: CacheMap,
    sessionCacheMap: Option[CacheMap])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[List[Section]] = {

    val countF = getRequestedCount(section.repeatsMax.get, formTemplate, data, cacheMap, sessionCacheMap)

    for {
      count <- countF
    } yield {
      (1 to count).map { i =>
        copySection(section, i, data, cacheMap)
      }.toList
    }
  }

  private def copySection(section: Section, index: Int, data: Map[FormComponentId, Seq[String]], cacheMap: CacheMap)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext) = {
    def copyField(field: FormComponent): FormComponent =
      field.`type` match {
        case grp @ Group(fields, _, _, _, _, _) =>
          field.copy(
            id = FormComponentId(s"${index}_${field.id.value}"),
            `type` = grp.copy(fields = fields.map(copyField))
          )
        case _ =>
          field.copy(
            id = FormComponentId(s"${index}_${field.id.value}")
          )
      }

    section.copy(
      title = buildText(Some(section.title), index, data, cacheMap).getOrElse(""),
      shortName = buildText(section.shortName, index, data, cacheMap),
      fields = section.fields.map(copyField)
    )
  }

  private def buildText(
    template: Option[String],
    index: Int,
    data: Map[FormComponentId, Seq[String]],
    cacheMap: CacheMap)(implicit hc: HeaderCarrier, ec: ExecutionContext): Option[String] = {

    def evaluateTextExpression(str: String) = {
      val field = str.replaceFirst("""\$\{""", "").replaceFirst("""\}""", "")
      if (field.startsWith("n_")) {
        if (index == 1) {
          val fieldName = field.replaceFirst("n_", "")
          data.getOrElse(FormComponentId(fieldName), Seq("")).mkString
        } else {
          val fieldName = field.replaceFirst("n_", s"${index - 1}_")
          data.getOrElse(FormComponentId(fieldName), Seq("")).mkString
        }
      } else {
        data.getOrElse(FormComponentId(field), Seq("")).mkString
      }
    }

    def getEvaluatedText(str: String) = {
      val pattern = """.*(\$\{.*\}).*""".r
      val expression = str match {
        case pattern(txtExpr) => txtExpr
        case _                => ""
      }
      val evaluatedText = evaluateTextExpression(expression)
      str.replace(expression, evaluatedText)
    }

    template match {
      case Some(inputText) => Some(getEvaluatedText(inputText).replace("$n", index.toString))
      case _               => None
    }
  }

  //This Evaluation is for the repeating sections, this will not become values.
  private def evaluateExpression(
    expr: Expr,
    formTemplate: FormTemplate,
    data: Map[FormComponentId, Seq[String]],
    sessionCacheMap: Option[CacheMap])(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Int] = {

    def evaluateExpression(expr: Expr): Future[Int] =
      expr match {
        case Add(expr1, expr2) =>
          for {
            first <- evaluateExpression(expr1)
            second <- evaluateExpression(expr2)
          } yield first + second
        case Multiply(expr1, expr2) =>
          for {
            first <- evaluateExpression(expr1)
            second <- evaluateExpression(expr2)
          } yield first * second
        case Subtraction(expr1, expr2) =>
          for {
            first <- evaluateExpression(expr1)
            second <- evaluateExpression(expr2)
          } yield first - second
        case Sum(FormCtx(expr1)) => sumFunctionality(expr1, formTemplate, data, sessionCacheMap)
        case formExpr@FormCtx(_) => Future.successful(getFormFieldIntValue(TextExpression(formExpr), data))
        case Constant(value) =>
          Try(value.toInt) match {
            case Success(intValue) => Future.successful(intValue)
            case _ => Future.successful(0)
          }
        //      case AuthCtx(value: AuthInfo) =>
        //      case EeittCtx(value: Eeitt) =>
        case _ => Future.successful(0)
      }

    evaluateExpression(expr)
  }

  private def sumFunctionality(
    expr1: String,
    formTemplate: FormTemplate,
    data: Map[FormComponentId, Seq[String]],
    sessionCacheMap: Option[CacheMap])(implicit hc: HeaderCarrier, ex: ExecutionContext) = {
    val dataGetter: FormComponentId => Int = fieldId =>
      Try(data.get(fieldId).toList.flatten.headOption.getOrElse("0").toInt).getOrElse(0)
    val cacheMap: CacheMap = getAllRepeatingGroups(sessionCacheMap)
    val repeatingSections: List[List[List[FormComponent]]] =
      formTemplate.sections.flatMap(_.fields).map(fv => (fv.id, fv.`type`)).collect {
        case (fieldId, group: Group) =>
          cacheMap.getEntry[RepeatingGroup](fieldId.value).map(_.list).getOrElse(Nil)
      }
    Group
      .getGroup(Future.successful(repeatingSections), FormComponentId(expr1))
      .flatMap(x => Future.successful(x.map(dataGetter).sum))
  }

  private def getRequestedCount(
    expr: TextExpression,
    formTemplate: FormTemplate,
    data: Map[FormComponentId, Seq[String]],
    cacheMap: CacheMap,
    sessionCacheMap: Option[CacheMap])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Int] = {

    val repeatingGroupsFound = findRepeatingGroupsContainingField(expr, formTemplate)

    if (repeatingGroupsFound.isEmpty) {
      evaluateExpression(expr.expr, formTemplate, data, sessionCacheMap)
    } else {
      val groupFieldValue = repeatingGroupsFound.head
      val fieldsInGroup = cacheMap.getEntry[RepeatingGroup](groupFieldValue.id.value).map(_.list).getOrElse(Nil).flatten
      Future.successful(fieldsInGroup.size)
    }
  }

  private def getFormFieldIntValue(expr: TextExpression, data: Map[FormComponentId, Seq[String]]): Int = {

    val id = extractFieldId(expr)

    data.get(FormComponentId(id)) match {
      case Some(value) =>
        Try(value.head.toInt) match {
          case Success(intValue) => intValue
          case _                 => 0
        }
      case None => 0
    }
  }

  private def extractFieldId(expr: TextExpression) =
    expr.expr match {
      case FormCtx(fieldId) => fieldId
      case _                => ""
    }

  private def findRepeatingGroupsContainingField(
    expr: TextExpression,
    formTemplate: FormTemplate): Set[FormComponent] = {

    val id = extractFieldId(expr)

    def findRepeatingGroups(groupField: Option[FormComponent], fieldList: List[FormComponent]): Set[FormComponent] =
      fieldList.flatMap { field =>
        field.`type` match {
          case Group(fields, _, repMax, _, _, _) if repMax.isDefined          => findRepeatingGroups(Some(field), fields)
          case othertype if groupField.isDefined && field.id.value.equals(id) => List(groupField.get)
          case _                                                              => Nil
        }
      }.toSet

    formTemplate.sections.flatMap(section => findRepeatingGroups(None, section.fields)).toSet
  }

  def appendNewGroup(formGroupId: String, sessionCacheMap: Option[CacheMap])(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Option[List[List[FormComponent]]]] = {
    // on the forms, the AddGroup button's name has the following format:
    // AddGroup-(groupFieldId)
    // that's the reason why the extraction below is required
    val startPos = formGroupId.indexOf('-') + 1
    val componentId = formGroupId.substring(startPos)

    def buildRepeatingGroup(dynamicList: List[List[FormComponent]], isRender: Boolean) = {
      val y = dynamicList match {
        case h :: Nil => addGroupEntry(dynamicList)
        case list     => addGroupEntry(list)
      }
      val x = if (isRender) y else dynamicList
      RepeatingGroup(x, render = true)
    }
    val dynamicListOpt = sessionCacheMap.flatMap(_.getEntry[RepeatingGroup](componentId))
    val dynamicList = dynamicListOpt.map(_.list).getOrElse(Nil) // Nil should never happen

    sessionCache
      .cache[RepeatingGroup](
        componentId,
        buildRepeatingGroup(dynamicList, dynamicListOpt.map(_.render).getOrElse(true)))
      .map(_.getEntry[RepeatingGroup](componentId).map(_.list))

  }

  def removeGroup(
    idx: Int,
    formGroupId: String,
    data: Map[FormComponentId, scala.Seq[String]],
    sessionCacheMap: Option[CacheMap])(implicit hc: HeaderCarrier, ec: ExecutionContext) = {
    // on the forms, the RemoveGroup button's name has the following format:
    // RemoveGroup-(groupFieldId)
    // that's the reason why the extraction below is required
    //    val groupIdStartPos = formGroupId.indexOf('-') + 1
    //    val componentId = formGroupId.substring(groupIdStartPos)
    //    val indexEndPos = componentId.indexOf("_")
    def index = if (idx == 1) 0 else idx - 1 //componentId.substring(0, indexEndPos).toInt
    val groupId = formGroupId //componentId.substring(indexEndPos + 1)

    def newListM(list: List[List[FormComponent]], oldList: List[List[FormComponent]]) =
      if (list.isEmpty)
        RepeatingGroup(oldList, false)
      else
        RepeatingGroup(list, true)

    def emptyCase(dynamicList: List[List[FormComponent]]): Map[FormComponentId, Seq[String]] = dynamicList match {
      case h :: Nil =>
        sessionCache.cache(groupId, newListM(Nil, dynamicList))
        data
      case list =>
        val (newList, newData) = renameFieldIdsAndData(list diff List(list(index)), data)
        sessionCache.cache[RepeatingGroup](groupId, newListM(newList, dynamicList))
        newData
    }
    for {
      dynamicListOpt <- sessionCacheMap.map(_.getEntry[RepeatingGroup](groupId))
      dynamicList = dynamicListOpt.map(_.list).getOrElse(Nil)
      newData = emptyCase(dynamicList)
    } yield newData
  }

  def getData(sessionCacheMap: Option[CacheMap]): Option[RepeatingGroupStructure] =
    sessionCacheMap.fold[Option[RepeatingGroupStructure]](None)(x => Some(RepeatingGroupStructure(x.data)))

  def loadData(data: Option[RepeatingGroupStructure])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    data.fold(Future.successful(()))(
      y =>
        Future.successful(
          y.structure.foreach(x =>
            x._2.asOpt[RepeatingGroup] match {
              case Some(z) => sessionCache.cache[RepeatingGroup](x._1, z)
              case None    => Map.empty
          })
      ))

  private def renameFieldIdsAndData(list: List[List[FormComponent]], data: Map[FormComponentId, scala.Seq[String]])
    : (List[List[FormComponent]], Map[FormComponentId, scala.Seq[String]]) = {

    var newData = data
    val result = (1 until list.size)
      .map { i =>
        list(i).map { field =>
          val newId = FormComponentId(buildNewId(field.id.value, i))
          newData = renameFieldInData(field.id, newId, newData)
          field.copy(id = newId)
        }
      }
      .toList
      .::(list(0))

    (result, newData)
  }

  private def renameFieldInData(
    src: FormComponentId,
    dst: FormComponentId,
    data: Map[FormComponentId, scala.Seq[String]]) =
    if (data.contains(src)) {
      val value = data(src)
      (data - src) + (dst -> value)
    } else {
      data
    }

  private def buildNewId(id: String, newIndex: Int) = {
    val endOfIndex = id.indexOf('_') + 1
    val idNoIndex = id.substring(endOfIndex)
    s"${newIndex}_$idNoIndex"
  }

  private def addGroupEntry(dynamicList: List[List[FormComponent]]) = {
    val countForNewEntry = dynamicList.size
    val newEntry = dynamicList(0).map { field =>
      field.copy(
        id = FormComponentId(s"${countForNewEntry}_${field.id.value}"),
        label = LabelHelper.buildRepeatingLabel(field, countForNewEntry + 1),
        shortName = LabelHelper.buildRepeatingLabel(field.shortName, countForNewEntry + 1)
      )
    }
    dynamicList :+ newEntry
  }

  private def isRepeatsMaxReached(count: Int, groupField: Group) =
    groupField.repeatsMax match {
      case Some(max) =>
        if (count >= max) {
          true
        } else {
          false
        }
      case None => true
    }

  def getRepeatingGroupsForRendering(
    topFieldValue: FormComponent,
    groupField: Group,
    sessionCacheMap: Option[CacheMap])(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[(List[List[FormComponent]], Boolean)] =
    sessionCacheMap.flatMap(_.getEntry[RepeatingGroup](topFieldValue.id.value)) match {
      case Some(dynamicList) if dynamicList.render =>
        Future.successful((dynamicList.list, isRepeatsMaxReached(dynamicList.list.size, groupField)))
      case Some(dynamicList) => Future.successful((Nil, false))
      case None              => initialiseDynamicGroupList(topFieldValue, groupField)
    }

  private def initialiseDynamicGroupList(parentField: FormComponent, group: Group)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext) = {
    val dynamicList = group.repeatsMin match {
      case Some(min) if min == 1 | min <= 0 => List(group.fields)
      case Some(min) if min > 1 =>
        group.fields +: (1 until min).map { i =>
          group.fields.map(
            field =>
              field.copy(
                id = FormComponentId(s"${i}_${field.id.value}"),
                label = LabelHelper.buildRepeatingLabel(field, i + 1),
                shortName = LabelHelper.buildRepeatingLabel(field.shortName, i + 1)
            ))
        }.toList //Not changing first Element to pass $n through repeated groups when adding new group
      case None => List(group.fields) //This should never happen only repeating groups get here.
    }

    val isRender: Boolean = group.repeatsMin.fold(true)(x => x != 0)
    sessionCache.cache[RepeatingGroup](parentField.id.value, RepeatingGroup(dynamicList, isRender)).map { _ =>
      if (isRender)
        (dynamicList, isRepeatsMaxReached(dynamicList.size, group))
      else
        (Nil, false)
    }
  }

  def getAllFieldsInGroup(
    topFieldValue: FormComponent,
    groupField: Group,
    sessionCacheMap: Option[CacheMap]): List[List[FormComponent]] = {
    val resultOpt = sessionCacheMap.flatMap(_.getEntry[RepeatingGroup](topFieldValue.id.value))
    resultOpt.map(_.list).getOrElse(List(groupField.fields))
  }

  def getAllFieldsInGroupForSummary(
    topFieldValue: FormComponent,
    groupField: Group,
    sessionCacheMap: Option[CacheMap]): List[List[FormComponent]] = {
    val resultOpt = sessionCacheMap.flatMap(_.getEntry[RepeatingGroup](topFieldValue.id.value))
    buildGroupFieldsLabelsForSummary(
      resultOpt.fold[List[List[FormComponent]]](List(groupField.fields))(x =>
        if (x.render) x.list else List(groupField.fields)),
      topFieldValue
    )
  }

  def clearSession(implicit hc: HeaderCarrier, ec: ExecutionContext) = sessionCache.remove()

  def atomicFields(section: BaseSection, sessionCacheMap: Option[CacheMap])(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): List[FormComponent] = {
    def atomicFields(fields: List[FormComponent]): List[FormComponent] =
      fields.flatMap {
        case (fv: FormComponent) =>
          fv.`type` match {
            case groupField @ Group(_, _, _, _, _, _) =>
              section match {
                case Section(_, _, _, _, _, _, _, _, _) =>
                  atomicFields {
                    val fields = getAllFieldsInGroup(fv, groupField, sessionCacheMap)
                    val first = fields.head.map { nv =>
                      nv.copy(
                        shortName = LabelHelper.buildRepeatingLabel(nv.shortName, 1),
                        label = LabelHelper.buildRepeatingLabel(nv, 1)
                      )
                    }
                    (first +: fields.tail).flatten
                  }
                case DeclarationSection(_, _, _, _) => atomicFields(groupField.fields)
                case _                              => None
              }
            case _ => List(fv)
          }
      }
    atomicFields(section.fields)
  }

  private def buildGroupFieldsLabelsForSummary(
    list: List[List[FormComponent]],
    fieldValue: FormComponent): List[List[FormComponent]] =
    (0 until list.size).map { i =>
      list(i).map { field =>
        field.copy(
          label = LabelHelper.buildRepeatingLabel(Some(field.label), i + 1).getOrElse(""),
          shortName = LabelHelper.buildRepeatingLabel(field.shortName, i + 1)
        )
      }
    }.toList
}
