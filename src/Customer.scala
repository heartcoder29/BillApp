import scala.collection.mutable.ListBuffer

class Customer(id: Int) {
  /**
   * Customer Class Constructor
   */
  val cusId: Int = id
  val UserName: String = Main.getTable("Select username from billdbschema.credentials where cusid=" + cusId).head
  var password: String = Main.getTable("Select password from billdbschema.credentials where cusid=" + cusId).head
  val Discount: String = Main.getTable("Select discount from billdbschema.credentials where cusid=" + cusId).head
  val role: String = "Customer"

  /**
   * Override of toString for Customer Objects
   *
   * @return
   */

  override def toString: String = "CustomerID => " + cusId + "\n" +
    "UserName => " + UserName + "\n" + "Password=>" +
    password + "\nDiscount=>" +
    Discount + "\n"

  /**
   * This Method Holds The Customer Related Operations
   * 1) place Order
   * 2) view History
   */
  def performCusOperation(): Unit = {
    var choice: Int = 0
    do {
      println("Hello Customer....," + this.UserName + "\nWelcome!!! " +
        "\n please Select the Operation to perform")
      println("=>Press 1 for Place an Order")
      println("=>Press 2 for View My Order History")
      println("=>Press 3 for LogOut/exit")
      println("Enter option")
      choice = scala.io.StdIn.readInt()
      choice match {
        case 1 =>
          this.placeAnOrder()
        case 2 =>
          println("History...")
          val temp = Main.getTable("Select date,statement from billdbschema.history where cusid=" + this.cusId + "order by date")
          println("Dear " + this.cusId + " Your History ...")
          printf("%-20s%-60s\n", "Date     ", "Statements")
          for (i <- temp) {
            val array = i.split(",", 2)
            History.printHistory(array(0), History.splitTransaction(array(1)))
          }
        case 3 =>

      }
    } while (choice <= 2)
  }

  /**
   * This Method Organize the order for Customer
   */

  def placeAnOrder(): Unit = {
    var pid = 0
    var quan = 0
    var avail = 0
    var price = 0
    var confirm = 0
    var discount = 0
    var sum = 0.0f
    val temp = new ListBuffer[String]()
    println("===>BILLING APPLICATION<===")
    println("NOTE: Dear " + this.UserName + " please See the table of Product and select accordingly....\n happy shopping....")
    Product.showDetails()
    do {
      println("Enter the Product ID(Valid)")
      println("Press 0 as INPUT for EXIT and go to cart menu")
      pid = scala.io.StdIn.readLine().trim.toInt
      if (!Main.isProductPresent(pid) || pid != 0) {
        println("Enter Quantity of " + Product.getProductInfo(pid))
        quan = scala.io.StdIn.readLine().trim.toInt
        avail = Main.getTable("Select quantity from billdbschema.input2 where pid=" + pid).head.filter(c => c != ',').trim.toInt
        if (avail < quan) {
          println(Product.getProductInfo(pid) + " is OutOfStock Current Available is" + avail)
        }
        else {
          price = Main.getTable("Select price from billdbschema.input2 where pid=" + pid).head.filter(c => c != ',').trim.toInt
          sum += (price * quan)
          temp += (pid + "/" + quan)
          println(f"Current Cart Price is =>$sum%.2f")
        }
      }
      else {
        println("Product ID that you have enter is not present")
        println("NOTE: Dear " + this.UserName + " please See the table of Product and select accordingly....\n happy shopping....")
      }
    } while (pid != 0)
    Product.printBill(this, temp.toList, discount = false)
    println("--- Please confirm the Order ---\n Press 1 for Jump to Cart & Get Items \n Press 0 to Cancel the Order")
    confirm = scala.io.StdIn.readInt()
    if (confirm == 0) {
      println("Your Order Canceled")
    }
    else {
      History.addRecord(this.cusId, confirmOrder(this.cusId, temp))
      if (this.Discount.equalsIgnoreCase("NOCOUPON,")) {
        println("You do not have any coupons for Discount\n your Bill,..")
        Product.printBill(this, temp.toList, discount = false)
      }
      else {
        println(this.UserName + "... Do you want to Apply Discount?")
        println("press 0 proceed without Discount")
        println("press 1 apply Discount")
        discount = scala.io.StdIn.readInt()
        if (discount == 1 && !this.Discount.equalsIgnoreCase("NOCOUPON,")) {
          Product.printBill(this, temp.toList, discount = true)
          Main.alterTable("update billdbschema.credentials set Discount = 'NOCOUPON' WHERE cusid = " + this.cusId)
        }
      }
    }
  }

  /**
   * This Method get the Discount Amount from the Coupons
   *
   * @return
   */
  def coupons(): Int = {
    if (this.Discount.equalsIgnoreCase("NOCOUPON,")) {
      0
    }
    else {
      this.Discount.toLowerCase().filter(x => x >= '0' && x <= '9').toInt
    }
  }

  /**
   * This Method confirm the Order and Update to DataBase
   *
   * @param cusId    customer ID
   * @param selected Selected Product Information
   * @return
   */
  def confirmOrder(cusId: Int, selected: ListBuffer[String]): List[String] = {
    val records = new ListBuffer[String]()
    for (i <- selected) {
      val temp = i.split("/+")
      Main.alterTable("update billdbschema.input2 set quantity = ((select quantity from  billdbschema.input2 where pid=" + temp(0) + ") - " + temp(1) + ") where pid=" + temp(0))
      Main.alterTable("update billdbschema.input2 set HIKE = ((select HIKE from  billdbschema.input2 where pid=" + temp(0) + ") + " + temp(1) + ") where pid=" + temp(0))
      records += ("'" + Product.getProductInfo(temp(0).trim.toInt) + "=> Quantity:" + temp(1) + "'")
    }
    println("(ID: " + cusId + " ) Your Order is confirmed...")
    records.toList
  }
}
