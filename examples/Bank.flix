namespace Bank {

    enum AccType {
        case Saving, case Cheqi, case Credit
    }

    rel Customer(name: String, address: String)

    rel Account(accNumber: Int, accName: String, customerName: String, amount: Int)

    rel Transfer(from: String)

    rel CreditDebitCards(...)

    rel Branch(address: String)

}