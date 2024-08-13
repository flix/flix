mod Test.Exp.Struct.Put {

    struct Put[r] {
        fstName: String,
        lstName: String,
        age: Int32,
        cowboy: Bool
    }

    @test
    def testStructPut01(): Bool =
        region rc {
            let s = new Put { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€fstName = "Unlucky";
            s€fstName == "Unlucky"
        }

    @test
    def testStructPut02(): Bool =
        region rc {
            let s = new Put { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€lstName = "Uriel";
            s€lstName == "Uriel"
        }

    @test
    def testStructPut03(): Bool =
        region rc {
            let s = new Put { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€age = 23;
            s€age == 23
        }

    @test
    def testStructPut04(): Bool =
        region rc {
            let s = new Put { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€cowboy = false;
            not s€cowboy and s€fstName == "Lucky" and s€lstName == "Luke" and s€age == 42
        }

    @test
    def testStructMultiPut01(): Bool =
        region rc {
            let s = new Put { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€fstName = "Unlucky";
            s€lstName = "Uriel";
            s€age = 23;
            s€cowboy = false;
            s€fstName == "Unlucky" and s€lstName == "Uriel" and s€age == 23 and s€cowboy == false
        }

    @test
    def testStructMultiPut02(): Bool =
        region rc {
            let s = new Put { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€cowboy = false;
            s€age = 23;
            s€lstName = "Uriel";
            s€fstName = "Unlucky";
            s€fstName == "Unlucky" and s€lstName == "Uriel" and s€age == 23 and s€cowboy == false
        }

    @test
    def testStructMultiPut03(): Bool =
        region rc {
            let s = new Put { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€fstName = "Unlucky";
            s€fstName = "Lucky";
            s€lstName = "Uriel";
            s€lstName = "Luke";
            s€lstName == "Luke" and s€fstName == "Lucky" and s€age == 42 and s€cowboy == true
        }

    @test
    def testStructMultiPut04(): Bool =
        region rc {
            let s = new Put { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€lstName = "Uriel";
            s€fstName = "Unlucky";
            s€lstName = "Luke";
            s€fstName = "Lucky";
            s€fstName == "Lucky" and s€lstName == "Luke" and s€age == 42 and s€cowboy == true
        }
}
