import scala.collection.mutable.ListBuffer

class Admin(id: Int) {
  /**
   * Admin Constructor
   */
  val cusId: Int = id
  val UserName: String = Main.getTable("Select username from billdbschema.credentials where cusid=" + cusId).head
  var password: String = Main.getTable("Select password from billdbschema.credentials where cusid=" + cusId).head
  val Discount: String = Main.getTable("Select discount from billdbschema.credentials where cusid=" + cusId).head
  val role: String = "Admin"

  /**
   * Override of toString for Admin Objects
   *
   * @return
   */
  override def toString: String = "AdminID => " + cusId + "\n" +
    "UserName => " + UserName + "\n" + "Password=>" +
    Main.encrypt(password) + "\nDiscount=>" +
    Discount + "\n"

  /**
   * This Method Holds The Admin Specific Operation
   * 1) Update operations
   * 2) view History of any
   * 3) view Products
   * 4) see top 3 selling items
   */
  def performAdminOperation(): Unit = {
    var choice = 0
    var records = new ListBuffer[String]()
    do {
      println("Hello Admin  " + this.UserName + "...,\nWelcome!!! " +
        "\nplease Select the Operation to perform")
      println("=>Press 1 for Update Operation")
      println("=>Press 2 for View History")
      println("=>Press 3 for View Products")
      println("=>Press 4 for Top selling")
      println("=>Press 5 for LogOut/exit")
      println("Enter option")
      choice = scala.io.StdIn.readInt()
      choice match {
        case 1 =>
          Product.showDetails()
          records = this.updateProducts()
        case 2 =>
          println("History...")
          println("Enter CusID to see the History ")
          val ch = scala.io.StdIn.readInt()
          if (Main.isPresent(ch)) {
            println("ID not found")
          }
          else {
            records += ("'Visited to See History of (Cus ID:" + ch + ")'")
            println("History...")
            val temp = Main.getTable("Select date,statement from billdbschema.history where cusid=" + ch + "order by date")
            println("Dear " + ch + " History ...")
            printf("%-20s%-60s\n", "Date     ", "Statements")
            for (i <- temp) {
              val array = i.split(",", 2)
              History.printHistory(array(0), History.splitTransaction(array(1)))
            }
          }
        case 3 =>
          println("================> PRODUCTS <=======" +
            "=============")
          Product.showDetails()
        case 4 =>
          println("Top 3 selling Items")
          Product.sortByHike()
        case _ =>
          History.addRecord(this.cusId, records.toList)
      }
    } while (choice <= 4)

  }

  /**
   * This Method Organize the Update Operations
   * 1) Add Products
   * 2) Update Price
   * 3) Update Quantity
   * 4) Remove Product
   *
   * @return
   */
  def updateProducts(): ListBuffer[String] = {
    val records = new ListBuffer[String]()
    var pid = 0
    var choice = 0
    var list1 = ""
    var quantity = 0
    var price = 0
    var cat = ""
    var pro = ""
    var dis = ""
    var strings = ""
    do {
      println("===============>Admin " +
        "Operations<==================")
      println("Hi Admin ,.. " + this.UserName + "\n Please Use " +
        "Underscore ,avoid space while enter Details...")

      println("1) Add New Product")
      println("2) Update Product Price")
      println("3) Update Product Quantity")
      println("4) Remove Products")
      println("5) History")
      println("6) Exit ")
      println("Enter your Choice:")
      choice = scala.io.StdIn.readInt()
      choice match {
        case 1 =>
          println("===============>Add new Product<====" +
            "=============")
          println("Enter the product ID")
          pid = scala.io.StdIn.readInt()
          println("Enter the product Name")
          pro = scala.io.StdIn.readLine()
          println("Enter the product Category")
          cat = scala.io.StdIn.readLine()
          println("Enter the Price per Unit")
          price = scala.io.StdIn.readInt()
          println("Enter the Quantity")
          quantity = scala.io.StdIn.readInt()
          println("Enter Discount Status * for Yes - for No")
          dis = scala.io.StdIn.readLine()

          if (Product.addNewProduct(pid, pro, cat, price, quantity, dis)) {
            println("Added... ")
            records += ("'successfully Added new product (ID:" + pid + ")'")
          } else {
            records += ("'Failed to Add new product (ID:" + pid + "'")
            println("Failed to Add")
          }
        case 3 =>
          list1 = ""
          var ch = 0
          Product.showDetails()
          strings = ""
          println("==============>Update Product Quantity<=" +
            "=" +
            "===============")
          do {
            println("Enter the product ID")
            pid = scala.io.StdIn.readInt()
            list1 += (pid + ",")
            println("Enter the Quantity")
            quantity = scala.io.StdIn.readInt()
            println("Added to Change List")
            strings += (pid + "-" + quantity + ",")
            println("Press 0 for Exit")
            println("Press 1 for Continue....")
            ch = scala.io.StdIn.readInt()
          } while (ch != 0)
          if (Product.updateStocks(strings.split(",+"), "quantity")) {
            println("Products Quantity are Updated")
            records += ("'Updated product Quantity for PIDs (" + list1 + ")'")
          }
          else {
            println("Failed to update")
          }
        case 2 =>
          var ch = 0
          list1 = ""
          Product.showDetails()
          strings = ""
          println("==============>Update Product price<===" +
            "==============")
          do {
            println("Enter the product ID")
            pid = scala.io.StdIn.readInt()
            list1 += (pid + ",")
            println("Enter the Price")
            price = scala.io.StdIn.readInt()
            println("Added to Change List")
            strings += (pid + "-" + price + ",")
            records += ("'Updated product Price for PIDs (" + list1 + ")'")
            println("Press 0 for Exit")
            ch = scala.io.StdIn.readInt()
          } while (ch != 0)
          if (Product.updateStocks(strings.split(",+"), "price")) {
            println("Products price are Updated")
          }
          else {
            println("Failed to Update")
          }
        case 4 =>
          Product.showDetails()
          println("==============Remove Product=====" +
            "============")
          println("Enter the product ID to Remove")
          pid = scala.io.StdIn.readInt()
          if (Product.removeProducts(pid)) {
            records += ("'PID : " + pid + "Removed Successfully'")
            println("Product ID " + pid + " Removed")
          }
          else {
            println("Unable remove / enter valid ID")
            records += ("'PID : " + pid + "Removed Failed'")
          }

        case 5 =>
          println("Enter the ID to Display History...")
          val Id = scala.io.StdIn.readInt()
          val temp = Main.getTable("Select date,statement from " +
            "billdbschema.history where cusid=" + Id + "order by date")
          println("Dear " + this.cusId + " Your History ...")
          printf("%-20s%-60s\n", "Date     ", "Statements")
          for (i <- temp) {
            val array = i.split(",", 2)
            History.printHistory(array(0), History.splitTransaction(array(1)))
          }
        case _ =>

      }
    } while (choice <= 4)
    records
  }


}
