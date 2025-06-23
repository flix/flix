/*
 *  Copyright 2025 Cade Lueker
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

///
///Represents a V6 Ip Address.
///
enum Ipv6Addr with Eq, ToString {
  case Ipv6Addr( // 128 bits
    Int8, Int8, Int8, Int8, // 32 bits
    Int8, Int8, Int8, Int8, // 32 bits
    Int8, Int8, Int8, Int8, // 32 bits
    Int8, Int8, Int8, Int8  // 32 bits
  )
}

