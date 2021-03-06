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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.ValueClassFormat

case class FormTemplateId(value: String) extends AnyVal

object FormTemplateId {

  implicit val mongoVformat: Format[FormTemplateId] =
    ValueClassFormat.vformat("_id", FormTemplateId.apply, x => JsString(x.value))
  val mongoOformat: OFormat[FormTemplateId] = ValueClassFormat.oformat("_id", FormTemplateId.apply, _.value)

  val vformat: Format[FormTemplateId] =
    ValueClassFormat.vformat("formTemplateId", FormTemplateId.apply, x => JsString(x.value))
  val oformat: OFormat[FormTemplateId] = ValueClassFormat.oformat("formTemplateId", FormTemplateId.apply, _.value)

}

// For Google Analytics only. Sole purpose of this value is to be present in URLs.
case class FormTemplateId4Ga(value: String) extends AnyVal

object FormTemplateId4Ga {

  implicit val format: Format[FormTemplateId4Ga] =
    uk.gov.hmrc.gform.models.ValueClassFormat.format(FormTemplateId4Ga.apply)(_.value)

}
