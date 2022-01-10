import java.sql.SQLException

object Product {

  /**
   * This Method Adds New Product
   *
   * @param pid      ID of the Product
   * @param pro      Name of thr Product
   * @param cat      Category of the Product
   * @param prices   Price of the Product per Unit
   * @param quan     Quantity of the Product
   * @param Discount Discount Status of the Product
   * @return
   */
  def addNewProduct(pid: Int, pro: String, cat: String, prices: Int, quan: Int, Discount: String): Boolean = {
    try {
      Main.alterTable("insert into billdbschema.input2 values(" + pid + ",'" + pro + "'" + ",'" + cat + "'," + prices + "," + quan + ",'" + Discount + "')")
      true
    }
    catch {
      case _: SQLException => false
    }
  }

  /**
   * This Method Remove The Product
   *
   * @param pid ID of the Product
   * @return
   */
  def removeProducts(pid: Int): Boolean = {
    if (Main.isPresent(pid)) {
      false
    }
    else {
      Main.alterTable("DELETE FROM billbdschema.input2 where pid = " + pid)
    }
  }

  /**
   * This Method Display the Products Available to Customer
   */
  def showDetails(): Unit = {
    printf("%1$-20s%2$-50s%3$-45s%4$-45s%5$-45s%6$-45s\n", "PID", "PRODUCT DETAILS",
      "PRICE", "QUANTITY AVAILABLE", "DISCOUNT", "HIKE")
    for (i <- Main.getTable("SELECT  * FROM billdbschema.input2 order by pid")) {
      val temp = i.split(",+")
      printf("%1$-20s%2$-50s%3$-45s%4$-45s%5$-45s%6$-45s\n", temp(0).toInt, temp(1) + "==>" + temp(2), temp(3).toInt, temp(4).toInt, temp(5), temp(6))
    }
  }

  /**
   * This Method Get Basic Information of the Product
   *
   * @param pid Product ID
   * @return
   */
  def getProductInfo(pid: Int): String = {
    val t = Main.getTable("SELECT PNAME,PCATEGORY " +
      "FROM billdbschema.input2 WHERE PID=" + pid).head.split(",+")
    t(0) + " Category: " + t(1)
  }

  /**
   * This Method Get Price per Unit of the Product
   *
   * @param pid Product ID
   * @return
   */

  def getPrice(pid: Int): Int = {
    Main.getTable("Select price from billdbschema.input2 " +
      "where pid=" + pid).head.filter(c => c != ',').trim.toInt
  }

  /**
   * This Method Get Available Quantity of the Product
   *
   * @param pid Product ID
   * @return
   */

  def getQuantity(pid: Int): Int = {
    Main.getTable("Select quantity from billdbschema.input2 " +
      "where pid=" + pid).head.filter(c => c != ',').trim.toInt
  }

  /**
   * This Method Update Stocks Availability for Specifed Column name
   *
   * @param update List to Update
   * @param column Column name
   * @return
   */

  def updateStocks(update: Array[String], column: String): Boolean = {
    for (i <- update) {
      val temp = i.split("-")
      val pid = temp(0).toInt
      val col_val = temp(1).toInt
      if (Main.isProductPresent(pid)) {
        return false
      }
      else {
        Main.alterTable("update billdbschema.input2 set " + column + " = " + col_val + " where pid=" + pid)
      }
    }
    true
  }

  /**
   * This Method get the Discount status for the Particular Product
   *
   * @param pid Product ID
   * @return
   */
  def getDiscount(pid: Int): Int = {
    Main.getTable("Select discount from billdbschema.input2 where pid=" + pid).head.filter(c => c != ',').trim.toInt
  }

  /**
   * This Method Print the Bill
   *
   * @param customer Customer Object the Invoke Order
   * @param pidArray List of Selected Item Information
   * @param discount Acknowledge for Discount from Customer
   * @return
   */

  def printBill(customer: Customer, pidArray: List[String], discount: Boolean): Double = {
    var total = 0
    var discounts = 0.0
    println("=================================" +
      "=============")
    println("************BILLING APPLICATION***************")
    println("Dear," + customer.UserName + "(ID:" + customer.cusId + ")")
    printf("|%-30s%-30s%-30s%-30s%-30s|\n", "Product Details",
      "Quantity", "Price per Unit", "Cost", "Discount")
    for (i <- pidArray) {
      val temp = i.split("/")
      val pid = temp(0).toInt
      val quantity = temp(1).toInt
      var dis = 0.0
      if (discount && isAvailDiscount(pid)) {
        dis = applyDiscount(pid, quantity, customer)
      }
      printf("|%-30s%-30d%-30d%-30d%-30s|\n", getProductInfo(pid),
        quantity, getPrice(pid), getPrice(pid) * quantity,
        discountStatus(discount, pid, dis))
      discounts += dis
      total += (getPrice(pid) * quantity)
    }
    printf(String.format("\n%76s%5d", "Total Amount =",
      Int.box(total)))
    printf(String.format("\n%78s%.2f", "Applied Discount =",
      Double.box(discounts)))
    printf("\n%87s", "-------------------------------")
    val grandTotal = total - discounts
    printf(String.format("\n%79s%.3f", "GRAND TOTAL:",
      Double.box(grandTotal)))
    printf("\n%87s", "-------------------------------\n")
    grandTotal
  }

  /**
   * This Method Check the Coupon Availability  of Particular Product
   *
   * @param pid Product ID
   * @return
   */
  def isAvailDiscount(pid: Int): Boolean = {
    Main.getTable("Select discount from billdbschema.input2 where pid=" + pid).head.filter(c => c != ',').trim.equals("-")
  }

  /**
   * This Method Returns the Status of Discount Applied to a Product in
   * Bill
   *
   * @param discount Availability of Discount Status for a Product
   * @param pid      Product ID
   * @param dis      Amount Of Discount
   * @return
   */
  def discountStatus(discount: Boolean, pid: Int, dis: Double): String = {
    if (discount && isAvailDiscount(pid)) String.format("   Applied ==> %.2f", dis)
    else "NotApplied"
  }

  /**
   * This Method Calculate Discount for Each Product
   *
   * @param pid      Product ID
   * @param quantity Quantity of product
   * @param customer Customer Object
   * @return
   */
  def applyDiscount(pid: Int, quantity: Int, customer: Customer): Double = {
    (getPrice(pid) * quantity) * (customer.
      coupons() / 100.0)
  }

  /**
   * This Method Sort the Product By Their Hike
   */
  def sortByHike(): Unit = {
    printf("%10s%40s\n", "HIKE", "PRODUCT DETAILS")
    for (i <- Main.getTable("SELECT HIKE, PNAME FROM billdbschema.input2 ORDER BY hike desc LIMIT 3")) {
      val temp = i.split(",+")
      printf("%10d%30s\n", temp(0).toInt, temp(1))
    }
  }
}
