@test
def trycatch01(): Bool =
    try true catch {
        case ex: ##java.lang.Throwable => false
    }

@test
def trycatch02(): Bool =
    try true catch {
        case ex: ##java.lang.Throwable => false
        case ex: ##java.lang.Exception => false
    }


@test
def trycatch03(): Bool =
    try true catch {
        case ex: ##java.lang.Throwable        => false
        case ex: ##java.lang.Exception        => false
        case ex: ##java.lang.RuntimeException => false
    }

@test
def trycatch04(): Bool =
    try true catch {
        case ex: ##java.lang.Throwable           => false
        case ex: ##java.lang.Exception           => false
        case ex: ##java.lang.RuntimeException    => false
        case ex: ##java.lang.ArithmeticException => false
    }

@test
def trycatch05(): Bool =
    try (0 / 0) == 0 catch {
        case ex: ##java.lang.ArithmeticException => true
    }

@test
def trycatch06(): Bool =
    try (0 / 0) == 0 catch {
        case ex: ##java.lang.RuntimeException => true
    }

@test
def trycatch07(): Bool =
    try (0 / 0) == 0 catch {
        case ex: ##java.lang.Exception => true
    }

@test
def trycatch08(): Bool =
    try (0 / 0) == 0 catch {
        case ex: ##java.lang.Throwable => true
    }

@test
def trycatch09(): Bool =
    try (0 / 0) == 0 catch {
        case ex: ##java.io.IOException           => false
        case ex: ##java.lang.ArithmeticException => true
    }

@test
def trycatch10(): Bool =
    try (0 / 0) == 0 catch {
        case ex: ##java.io.IOException            => false
        case ex: ##java.lang.NullPointerException => false
        case ex: ##java.lang.ArithmeticException  => true
    }

@test
def trycatch11(): Bool =
    try (0 / 0) == 0 catch {
        case ex: ##java.io.IOException            => false
        case ex: ##java.lang.NullPointerException => false
        case ex: ##java.lang.InterruptedException => false
        case ex: ##java.lang.ArithmeticException  => true
    }

@test
def trycatch13(): Bool =
    try (0 / 0) == 0 catch {
        case ex: ##java.lang.ArithmeticException =>
            let x = true;
                x
    }

@test
def trycatch14(): Bool =
    try (0 / 0) == 0 catch {
        case ex: ##java.lang.ArithmeticException =>
            let x = true;
            let y = false;
            let z = true;
                x && z
    }

@test
def trycatch15(): Bool =
    try (0 / 0) == 0 catch {
        case ex: ##java.lang.ArithmeticException =>
            let x = 0.0;
            let y = 123;
            let z = 0.5;
                true
    }

//@test // TODO: Currently broken.
def trytrycatchcatch01(): Bool =
    try {
        try (0 / 0) == 0 catch {
            case ex: ##java.lang.ArithmeticException => true
        }
    } catch {
         case ex: ##java.lang.ArithmeticException => false
     }

@test
def trycatchtrycatch01(): Bool =
    try (0 / 0) == 0 catch {
        case ex: ##java.lang.ArithmeticException =>
            try (0 / 0) == 0 catch {
                case ex: ##java.lang.ArithmeticException => true
            }
    }

@test
def trycatchtrycatch02(): Bool =
    try (0 / 0) == 0 catch {
        case ex: ##java.lang.ArithmeticException =>
            try (0 / 0) == 0 catch {
                case ex: ##java.lang.ArithmeticException =>
                    try (0 / 0) == 0 catch {
                        case ex: ##java.lang.ArithmeticException => true
                    }
            }
    }
