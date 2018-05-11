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

import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.cache.client.SessionCache
import uk.gov.hmrc.http.{ HttpDelete, HttpGet, HttpPut }

class SessionCacheConnector(
  sourceName: String,
  baseUrl: String,
  val domain: String,
  wSHttp: WSHttp
) extends SessionCache {

  override def defaultSource: String = sourceName

  override def baseUri: String = baseUrl

  override def http: HttpGet with HttpPut with HttpDelete = wSHttp
}
