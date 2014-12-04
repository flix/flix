namespace Bool {

    type Belnap = enum {
        case Top,
        case True, case False,
        case Bot
    };

}

//(def-type Boolean (variant ((Top) (True) (False) (Bot))))
//
//(def-type A Boolean)
//(def-type B Boolean)
//(def-type R Boolean)
//
//(def-bot Boolean Bot)
//
//(def-leq Boolean (e1 Boolean e2 Boolean)
//    (match <e1 e2>
//        (case <Bot _>         true)
//        (case <True True>     true)
//        (case <False False>   true)
//        (case <_ Top>         true)
//        (case _               false)))
//
//(def-lub Boolean (e1 Boolean e2 Boolean)
//    (match <e1 e2>
//        (case <Bot x>         x)
//        (case <x Bot>         x)
//        (case <True True>     True)
//        (case <False False>   False)
//        (case _               Top)))
//
//(def-height Boolean (e Boolean)
//    (match e
//        (case Top      1)
//        (case True     2)
//        (case False    2)
//        (case Bot      3)))
//
//(fact (A True))
//(fact (B False))
//
//(rule (R x) ((A x)))
//(rule (R x) ((B x)))

