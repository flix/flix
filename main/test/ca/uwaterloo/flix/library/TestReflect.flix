mod TestReflect {

    use Assert.{assertEq, assertTrue};

    import java.lang.{Object => JObject}
    import java.util.Arrays

    @Test
    def defaultUnit01(): Unit \ Assert =
        assertEq(expected = (), (Reflect.default(): Unit))

    @Test
    def defaultBool01(): Unit \ Assert =
        assertEq(expected = false, (Reflect.default(): Bool))

    @Test
    def defaultChar01(): Unit \ Assert =
        assertEq(expected = '0', (Reflect.default(): Char))

    @Test
    def defaultFloat3201(): Unit \ Assert =
        assertEq(expected = 0.0f32, (Reflect.default(): Float32))

    @Test
    def defaultFloat6401(): Unit \ Assert =
        assertEq(expected = 0.0f64, (Reflect.default(): Float64))

    @Test
    def defaultBigDecimal01(): Unit \ Assert =
        assertTrue(Object.isNull((Reflect.default(): BigDecimal)))

    @Test
    def defaultInt801(): Unit \ Assert =
        assertEq(expected = 0i8, (Reflect.default(): Int8))

    @Test
    def defaultInt1601(): Unit \ Assert =
        assertEq(expected = 0i16, (Reflect.default(): Int16))

    @Test
    def defaultInt3201(): Unit \ Assert =
        assertEq(expected = 0i32, (Reflect.default(): Int32))

    @Test
    def defaultInt6401(): Unit \ Assert =
        assertEq(expected = 0i64, (Reflect.default(): Int64))

    @Test
    def defaultBigInt01(): Unit \ Assert =
        assertTrue(Object.isNull((Reflect.default(): BigInt)))

    @Test
    def defaultList01(): Unit \ Assert =
        assertTrue(Object.isNull((Reflect.default(): List[String])))

    @Test
    def reflectTypeBool01(): Unit \ Assert = region rc {
        let a = Array#{true, false, true} @ rc;
        assertEq(expected = false :: true :: Nil, Array.toList(copyOfRange(rc, 1, 3, a)))
    }

    @Test
    def reflectTypeChar01(): Unit \ Assert = region rc {
        let a = Array#{'a', 'b', 'c'} @ rc;
        assertEq(expected = 'b' :: 'c' :: Nil, Array.toList(copyOfRange(rc, 1, 3, a)))
    }

    @Test
    def reflectTypeInt801(): Unit \ Assert = region rc {
        let a = Array#{1i8, 2i8, 3i8} @ rc;
        assertEq(expected = 2i8 :: 3i8 :: Nil, Array.toList(copyOfRange(rc, 1, 3, a)))
    }

    @Test
    def reflectTypeInt1601(): Unit \ Assert = region rc {
        let a = Array#{1i16, 2i16, 3i16} @ rc;
        assertEq(expected = 2i16 :: 3i16 :: Nil, Array.toList(copyOfRange(rc, 1, 3, a)))
    }

    @Test
    def reflectTypeInt3201(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3} @ rc;
        assertEq(expected = 2 :: 3 :: Nil, Array.toList(copyOfRange(rc, 1, 3, a)))
    }

    @Test
    def reflectTypeInt6401(): Unit \ Assert = region rc {
        let a = Array#{1i64, 2i64, 3i64} @ rc;
        assertEq(expected = 2i64 :: 3i64 :: Nil, Array.toList(copyOfRange(rc, 1, 3, a)))
    }

    @Test
    def reflectTypeFloat3201(): Unit \ Assert = region rc {
        let a = Array#{1.0f32, 2.0f32, 3.0f32} @ rc;
        assertEq(expected = 2.0f32 :: 3.0f32 :: Nil, Array.toList(copyOfRange(rc, 1, 3, a)))
    }

    @Test
    def reflectTypeFloat6401(): Unit \ Assert = region rc {
        let a = Array#{1.0f64, 2.0f64, 3.0f64} @ rc;
        assertEq(expected = 2.0f64 :: 3.0f64 :: Nil, Array.toList(copyOfRange(rc, 1, 3, a)))
    }

    @Test
    def reflectTypeObject01(): Unit \ Assert = region rc {
        let a = Array#{"a", "b", "c"} @ rc;
        assertEq(expected = "b" :: "c" :: Nil, Array.toList(copyOfRange(rc, 1, 3, a)))
    }

    ///
    /// Returns a copy of `a` from `b` (inclusive) to `e` (exclusive).
    ///
    /// Selects the overload of `Arrays.copyOfRange` by reflecting on the element type.
    /// If `Reflect.reflectType` returns the wrong type, the cast of `a` fails at runtime.
    ///
    def copyOfRange(_: Region[r2], b: Int32, e: Int32, a: Array[a, r1]): Array[a, r2] \ { r1, r2 } = {
        use Reflect.JvmType;
        match Reflect.reflectType((Proxy.Proxy: Proxy[a])) {
            case JvmType.JvmBool =>
                let arr = unchecked_cast(a as Array[Bool, r1]);
                unchecked_cast(Arrays.copyOfRange(arr, b, e) as Array[a, r2] \ { r1, r2 })
            case JvmType.JvmChar =>
                let arr = unchecked_cast(a as Array[Char, r1]);
                unchecked_cast(Arrays.copyOfRange(arr, b, e) as Array[a, r2] \ { r1, r2 })
            case JvmType.JvmInt8 =>
                let arr = unchecked_cast(a as Array[Int8, r1]);
                unchecked_cast(Arrays.copyOfRange(arr, b, e) as Array[a, r2] \ { r1, r2 })
            case JvmType.JvmInt16 =>
                let arr = unchecked_cast(a as Array[Int16, r1]);
                unchecked_cast(Arrays.copyOfRange(arr, b, e) as Array[a, r2] \ { r1, r2 })
            case JvmType.JvmInt32 =>
                let arr = unchecked_cast(a as Array[Int32, r1]);
                unchecked_cast(Arrays.copyOfRange(arr, b, e) as Array[a, r2] \ { r1, r2 })
            case JvmType.JvmInt64 =>
                let arr = unchecked_cast(a as Array[Int64, r1]);
                unchecked_cast(Arrays.copyOfRange(arr, b, e) as Array[a, r2] \ { r1, r2 })
            case JvmType.JvmFloat32 =>
                let arr = unchecked_cast(a as Array[Float32, r1]);
                unchecked_cast(Arrays.copyOfRange(arr, b, e) as Array[a, r2] \ { r1, r2 })
            case JvmType.JvmFloat64 =>
                let arr = unchecked_cast(a as Array[Float64, r1]);
                unchecked_cast(Arrays.copyOfRange(arr, b, e) as Array[a, r2] \ { r1, r2 })
            case JvmType.JvmObject =>
                let arr = unchecked_cast(a as Array[JObject, r1]);
                unchecked_cast(Arrays.copyOfRange(arr, b, e) as Array[a, r2] \ { r1, r2 })
        }
    }

}
