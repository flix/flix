namespace Bank {

    enum AccountType {
        case Saving, case Cheqi, case Credit
    }

    enum CardType {
        case Debit, Visa, MasterCard
    }

    rel Customer(name: String, address: String)

    rel Account(accountNumber: Int, customerName: String, accountName: String, amount: Int)

    rel Transfer(from: String)

    rel Card(cardNumber: Int, ...)

    rel Branch(address: String)

    Customer("Rufus Nelson", "Dybedalen 1").

    Account(42, "Rufus Nelson", "My Carefree Chequing", 5000).
    Account(43, "Rufus Nelson", "My Savings", 3000).
    Account(44, "Rufus Nelson", "Credit", -2000).

    Card().

}