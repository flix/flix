@test
def select_int_channel_first(): Bool =
    ch1 = channel Int;
    let r = select {
        case x = <- ch1 => x + 1
        case x = <- ch2 => x + 2
    };
    ch1 <- 1;
    r `assertEq!` 2

@test
def select_int_channel_last(): Bool =
    ch2 = channel Int;
    let r = select {
        case x = <- ch1 => x + 1
        case x = <- ch2 => x + 2
    };
    ch1 <- 1;
    r `assertEq!` 3

@test
def select_string_channel_first(): Bool =
    ch1 = channel Str;
    let r = select {
        case x = <- ch1 => x + "1"
        case x = <- ch2 => x + "2"
    };
    ch1 <- "str";
    r `assertEq!` "str1"

@test
def select_string_channel_first(): Bool =
    ch1 = channel Str;
    let r = select {
        case x = <- ch1 => x + "1"
        case x = <- ch2 => x + "2"
    };
    ch2 <- "str";
    r `assertEq!` "str2"
