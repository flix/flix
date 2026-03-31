/*
 * Copyright 2025 Magnus Madsen
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

pub mod MutHashMap.Entry {
    pub def getKey(e: Entry[k, v, r]): k = e->key
    pub def getValue(e: Entry[k, v, r]): v \ r = e->value
    pub def setValue(v: v, e: Entry[k, v, r]): Unit \ r = e->value = v
    pub def getHash(e: Entry[k, v, r]): Int32 = e->hash
    pub def getBucketNext(e: Entry[k, v, r]): Option[Entry[k, v, r]] \ r = e->bucketNext
    pub def setBucketNext(next: Option[Entry[k, v, r]], e: Entry[k, v, r]): Unit \ r = e->bucketNext = next
    pub def getOrderPrev(e: Entry[k, v, r]): Option[Entry[k, v, r]] \ r = e->orderPrev
    pub def setOrderPrev(prev: Option[Entry[k, v, r]], e: Entry[k, v, r]): Unit \ r = e->orderPrev = prev
    pub def getOrderNext(e: Entry[k, v, r]): Option[Entry[k, v, r]] \ r = e->orderNext
    pub def setOrderNext(next: Option[Entry[k, v, r]], e: Entry[k, v, r]): Unit \ r = e->orderNext = next
}
