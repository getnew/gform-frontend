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

package uk.gov.hmrc.gform

import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, FormTemplateId4Ga, Sterling, Text }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponent

package object ops {

  implicit class FormTemplateIdSyntax(formTemplateId: FormTemplateId) {
    def to4Ga: FormTemplateId4Ga = FormTemplateId4Ga(formTemplateId.value)
  }

  implicit class FormComponentOps(formComponent: FormComponent) {
    def isSterling = formComponent.`type` match {
      case Text(Sterling, _) => true
      case _                 => false
    }
  }

}
