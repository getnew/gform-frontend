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

import cats.data._
import cats.implicits._
import play.api.Logger
import uk.gov.hmrc.gform.keystore.RepeatingService.RepeatingStructure
import uk.gov.hmrc.gform.sharedmodel.Shape
import uk.gov.hmrc.gform.sharedmodel.form.RepeatingGroupStructure
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class RepeatProxy(
    repeatingComponentService: RepeatingComponentService,
    isKeyStore: Boolean
) {

  val repeatingService = RepeatingService

  def getAllSections(formTemplate: FormTemplate, data: Map[FormComponentId, Seq[String]])(implicit hc: HeaderCarrier, executionContext: ExecutionContext): Future[List[Section]] = {
    if (isKeyStore)
      repeatingComponentService.getAllSections(formTemplate, data)
    else
      repeatingService.getAllSections(formTemplate, data)
  }

  def getAllRepeatingGroups(shape: Shape, formTemplate: FormTemplate)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[CacheMap] = {
    if (isKeyStore)
      repeatingComponentService.getAllRepeatingGroups
    else
      repeatingService.getAllRepeatingGroups(shape, formTemplate)
  }

  def atomicFields(section: BaseSection, shape: Shape, formTemplate: FormTemplate)(implicit hc: HeaderCarrier, ec: ExecutionContext): List[FormComponent] = {
    if (isKeyStore)
      repeatingComponentService.atomicFields(section)
    else
      repeatingService.getSectionFields(section, shape)
  }

  def getData(shape: Shape, formTemplate: FormTemplate)(implicit hc: HeaderCarrier, ec: ExecutionContext): (Shape, Future[Option[RepeatingGroupStructure]]) = {
    val dF: Future[Option[RepeatingGroupStructure]] = if (isKeyStore)
      repeatingComponentService.getData()
    else
      repeatingService.getAllRepeatingGroups(shape, formTemplate).map(y => RepeatingGroupStructure(y.data).some)
    (shape, dF.map {
      case d: Option[RepeatingGroupStructure] => {
        Logger.debug(s"""shape is ${shape}""")
        Logger.debug(s"""getData is ${d.toString}""")
        d
      }
    })
  }

  def loadData(data: Option[RepeatingGroupStructure])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] = {
    if (isKeyStore)
      repeatingComponentService.loadData(data)
    else
      ().pure[Future]
  }

  def getAllFieldsInGroup(fc: FormComponent, group: Group, formTemplate: FormTemplate, shape: Shape)(implicit hc: HeaderCarrier, ec: ExecutionContext): RepeatingStructure = {
    val rF = if (isKeyStore)
      repeatingComponentService.getAllFieldsInGroup(fc, group)
    else
      repeatingService.getRepeatingGroup(fc.id, formTemplate, shape)
    rF.map {
      case r: RepeatingStructure => {
        Logger.debug(s"repeatingStructure is ${r.toString}")
        r
      }
    }
  }

  private def isMax(max: Option[Int], size: Int): Boolean = {
    max match {
      case Some(m) => size == m
      case None => false
    }
  }

  def getRepeatingGroupsForRendering(formComponent: FormComponent, group: Group, formTemplate: FormTemplate, shape: Shape)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[(RepeatingStructure, Boolean)] = {
    val xF = if (isKeyStore)
      repeatingComponentService.getRepeatingGroupsForRendering(formComponent, group)
    else
      repeatingService.getRepeatingGroup(formComponent.id, formTemplate, shape).
        pure[Future].
        map(x => (x, isMax(group.repeatsMax, x.size)))
    xF.map {
      case (r, b) => {
        Logger.debug(s"ForRendering repeatingStructure is ${r.toString} and ${b}")
        (r, b)
      }
    }
    xF
  }

  def getAllFieldsInGroupForSummary(formComponent: FormComponent, group: Group, formTemplate: FormTemplate, shape: Shape)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[RepeatingStructure] = {
    if (isKeyStore)
      repeatingComponentService.getAllFieldsInGroupForSummary(formComponent, group)
    else
      repeatingService.getRepeatingGroup(formComponent.id, formTemplate, shape).pure[Future]
  }

  def clearSession(implicit hc: HeaderCarrier, ec: ExecutionContext) =
    if (isKeyStore)
      repeatingComponentService.clearSession
    else
      ().pure[Future]

  def appendNewGroup(formComponentId: FormComponentId, shape: Shape, formTemplate: FormTemplate)(implicit hc: HeaderCarrier, ec: ExecutionContext): (Shape, Future[Option[RepeatingStructure]]) = {
    if (isKeyStore)
      shape -> repeatingComponentService.appendNewGroup(formComponentId.value)
    else
      repeatingService.addGroup(shape, formComponentId, formTemplate)
  }

  def removeGroup(idx: Int, formComponentId: FormComponentId, data: Map[FormComponentId, Seq[String]], shape: Shape, formTemplate: FormTemplate)(implicit hc: HeaderCarrier, ec: ExecutionContext): (Shape, Future[Map[FormComponentId, Seq[String]]]) = {
    if (isKeyStore)
      shape -> repeatingComponentService.removeGroup(idx, formComponentId.value, data)
    else
      repeatingService.removeGroup(shape, formComponentId, data, formTemplate, idx)
  }
}
