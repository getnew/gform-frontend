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

/*
package uk.gov.hmrc.gform.models

import org.jsoup.Jsoup
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar.mock
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.auth.models.Retrievals
import uk.gov.hmrc.gform.keystore.SessionCacheConnector
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.gform.PrepopService
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.collection.immutable.List
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }

class SectionRenderingInformationSpec extends Spec {

  val markdown =
    """
      |You are now seeing markdown.
      |
      |# This is an H1
      |## This is an H2
      |### This is an H3
      |
      |Link:
      |
      |[This is a link](https://avatars1.githubusercontent.com/u/5755339?v=3&s=200)
      |
      |An image: ![Alt Text](/template/assets/images/gov.uk_logotype_crown.png)
      |
      |Now some code:
      |```
      |This is some code
      |Second line of code
      |Third line of code
      |```
      |
      |Ordered list
      |
      |1. One
      |2. Two
      |3. Three
      |
      |Unordered list
      |
      |* element one
      |* element two
      |* element three
      |
      || Table         | Col           | name  |
      || ------------- |:-------------:| -----:|
      || col 3 is      | right-aligned | $1600 |
      || col 2 is      | centered      |   $12 |
      || aa a a a a aa | bbbbbbbbb     |    $1 |
      |
      """.stripMargin

  val infoFieldValue = FieldValue(
    id = FieldId("testInfoField"),
    `type` = InformationMessage(StandardInfo, markdown),
    label = "This is the field label",
    helpText = None,
    shortName = None,
    mandatory = true,
    editable = false,
    submissible = false,
    errorMessage = None
  )

  override val dmsSubmission = DmsSubmission("Dunno", "pure class", "pure business")
  val section = Section("About you", None, None, None, None, None, List(infoFieldValue))

  val mockPrepopService = new PrepopService(null, null) {
    override def prepopData(expr: Expr, formTemplateId: FormTemplateId)(implicit retrievals: Retrievals, hc: HeaderCarrier): Future[String] =
      Future.successful("CONSTANT_TEXT")
  }

  override val formTemplate = FormTemplate(
    _id = FormTemplateId(""),
    formName = "AAA000",
    description = "YEAHH man!",
    formCategory = Some(Default),
    dmsSubmission = dmsSubmission,
    authConfig = AuthConfig(AuthConfigModule("TEST"), None, RegimeId("TEST"), None),
    submitSuccessUrl = "success-url",
    submitErrorUrl = "error-url",
    sections = List(section),
    List.empty[AckSection]
  )

  val mockRepeatService = mock[RepeatingComponentService]
  val envelope = Envelope(files = Nil)
  override val envelopeId = EnvelopeId("env-id")
  implicit val hc = HeaderCarrier()
  implicit val mockAuthRetrievals = mock[Retrievals]
  override val formId = FormId("formid-123")
  val sectionNumber = SectionNumber(0)

  "PageForRender for info field" should "return the HMTL representation of provided markdown" in {
    when(mockRepeatService.getAllSections(any(), any())(any())).thenReturn(Future.successful(formTemplate.sections))

    val pageToRenderF = SectionRenderingInformation(
      formId,
      sectionNumber,
      fieldData = Map.empty[FieldId, Seq[String]],
      formTemplate = formTemplate,
      f = None,
      mockRepeatService,
      envelope,
      envelopeId,
      mockPrepopService,
      formTemplate.sections,
      10,
      Nil
    )

    val pageToRender = Await.result(pageToRenderF, 10 seconds)
    val doc = Jsoup.parse(pageToRender.snippets.head.toString)

    doc.getElementsByTag("H1").size() shouldBe 1
    doc.getElementsByTag("H2").size() shouldBe 1
    doc.getElementsByTag("H3").size() shouldBe 1
    doc.getElementsByTag("A").size() shouldBe 1
    doc.getElementsByTag("IMG").size() shouldBe 1
    doc.getElementsByTag("CODE").size() shouldBe 1
    doc.getElementsByTag("PRE").size() shouldBe 1
    doc.getElementsByTag("OL").size() shouldBe 1
    doc.getElementsByTag("UL").size() shouldBe 1

    val table = doc.getElementsByTag("TABLE")
    table.size() shouldBe 1
    table.first.getElementsByTag("TR").size shouldBe 4
    table.first.getElementsByTag("TD").size shouldBe 9
  }

  //  GROUPS

  val grpTextField = FieldValue(
    id = FieldId("INNER_TEXT_FIELD"),
    `type` = Text(AnyText, Constant("CONSTANT_TEXT")),
    label = "INNER_TEXT_LABEL",
    shortName = None,
    helpText = None,
    mandatory = true,
    editable = true,
    submissible = true,
    errorMessage = None
  )
  val groupFields = List(grpTextField)

  val group = Group(
    fields = groupFields,
    orientation = Vertical,
    repeatsMax = Some(3),
    repeatsMin = Some(1),
    repeatLabel = Some("REPEAT_LABEL"),
    repeatAddAnotherText = Some("TEXT_IN_ADD_BUTTON")
  )

  val groupFieldValue = FieldValue(
    id = FieldId("GROUP_ID"),
    `type` = group,
    label = "LABEL_GROUP_FIELD",
    helpText = None,
    shortName = None,
    mandatory = true,
    editable = false,
    submissible = false,
    errorMessage = None
  )

  val grpSection = section.copy(fields = List(groupFieldValue))

  val mockSession = mock[SessionCacheConnector]

  "PageForRender for group field" should "return HTML with dynamic groups and an add-group button (repeating groups)" in {

    val testGrpRepSrvc = new RepeatingComponentService(mockSession) {
      override def getAllFieldsInGroup(topFieldValue: FieldValue, groupField: Group)(implicit hc: HeaderCarrier) = {
        multipleCopiesOf(grpTextField, 2)
      }

      override def getRepeatingGroupsForRendering(topFieldValue: FieldValue, groupField: Group)(implicit hc: HeaderCarrier) = {
        Future.successful((List(
          multipleCopiesOf(grpTextField, 1),
          multipleCopiesOf(grpTextField, 1)
        ), false))
      }

      override def getAllSections(formTemplate: FormTemplate, data: Map[FieldId, Seq[String]])(implicit hc: HeaderCarrier) = {
        Future.successful(List(grpSection))
      }
    }

    val pageToRenderF = SectionRenderingInformation(
      formId,
      SectionNumber(0),
      fieldData = Map.empty[FieldId, Seq[String]],
      formTemplate = formTemplate.copy(sections = List(grpSection)),
      f = None,
      testGrpRepSrvc,
      envelope,
      envelopeId,
      mockPrepopService,
      List(grpSection),
      10,
      Nil
    )

    val pageToRender = Await.result(pageToRenderF, 10 seconds)
    val doc = Jsoup.parse(pageToRender.snippets.head.toString)

    doc.getElementsByAttributeValue("value", "AddGroup-GROUP_ID").size shouldBe 1 withClue "no limit reached, add button shown"
    doc.getElementsByAttributeValue("value", "CONSTANT_TEXT").size shouldBe 2 withClue "two repeat elements"
    doc.getElementsContainingOwnText("REPEAT_LABEL").size shouldBe 2 withClue "repeat label only for second element"
  }

  it should "hide add-group button when limit has been reached (repeating groups)" in {
    val testGrpRepSrvc = new RepeatingComponentService(mockSession) {
      override def getAllFieldsInGroup(topFieldValue: FieldValue, groupField: Group)(implicit hc: HeaderCarrier) = {
        multipleCopiesOf(grpTextField, 2)
      }

      override def getRepeatingGroupsForRendering(topFieldValue: FieldValue, groupField: Group)(implicit hc: HeaderCarrier) = {
        Future.successful((List(
          multipleCopiesOf(grpTextField, 1),
          multipleCopiesOf(grpTextField, 1)
        ), true))
      }

      override def getAllSections(formTemplate: FormTemplate, data: Map[FieldId, Seq[String]])(implicit hc: HeaderCarrier) = {
        Future.successful(List(grpSection))
      }
    }

    val pageToRenderF = SectionRenderingInformation(
      formId,
      SectionNumber.firstSection,
      fieldData = Map.empty[FieldId, Seq[String]],
      formTemplate = formTemplate.copy(sections = List(grpSection)),
      f = None,
      testGrpRepSrvc,
      envelope,
      envelopeId,
      mockPrepopService,
      List(grpSection),
      10,
      Nil
    )

    val pageToRender = Await.result(pageToRenderF, 10 seconds)
    val doc = Jsoup.parse(pageToRender.snippets.head.toString)

    doc.getElementsByAttributeValue("value", "AddGroup-GROUP_ID").size shouldBe 0 // no add label when limit reached
    doc.getElementsByAttributeValue("value", "CONSTANT_TEXT").size shouldBe 2
  }

  private def multipleCopiesOf(fieldValue: FieldValue, count: Int): List[FieldValue] = {
    (1 to count).map { i =>
      fieldValue.copy(id = FieldId(s"${i}_${fieldValue.id.value}"))
    }.toList
  }

}
 */
