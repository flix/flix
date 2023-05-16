/*
 * Copyright 2023 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

mod TestCodePoint {

/////////////////////////////////////////////////////////////////////////////
// getNumericValue                                                         //
/////////////////////////////////////////////////////////////////////////////

@test
def getNumericValue01(): Bool = CodePoint.getNumericValue(0x0030) == Some(0)       // '0'

@test
def getNumericValue02(): Bool = CodePoint.getNumericValue(0x0031) == Some(1)       // '1'

@test
def getNumericValue03(): Bool = CodePoint.getNumericValue(0x0039) == Some(9)       // '9'

@test
def getNumericValue04(): Bool = CodePoint.getNumericValue(0x0041) == Some(10)      // 'A'

@test
def getNumericValue05(): Bool = CodePoint.getNumericValue(0x0041) == Some(10)      // 'a'

@test
def getNumericValue06(): Bool = CodePoint.getNumericValue(0x0046) == Some(15)      // 'F'

@test
def getNumericValue07(): Bool = CodePoint.getNumericValue(0x0066) == Some(15)      // 'f'

@test
def getNumericValue08(): Bool = CodePoint.getNumericValue(0x0020) == None          // ' ' (space)

@test
def getNumericValue09(): Bool = CodePoint.getNumericValue(0x002b) == None          // '+'

@test
def getNumericValue10(): Bool = CodePoint.getNumericValue(0x002d) == None          // '-'

@test
def getNumericValue11(): Bool = CodePoint.getNumericValue(0x1f3dd) == None          // Desert Island pictogram

}
