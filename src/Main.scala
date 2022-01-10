import java.sql.{Connection, DriverManager, SQLException}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.Locale
import scala.collection.mutable.ListBuffer

object Main {
  def main(args: Array[String]): Unit = {
    var ch = 0
    do {
      login()
      println("=>Press 1 to continue")
      println("=>Press 0 to exit")
      ch = scala.io.StdIn.readInt()
    } while (ch != 0)
  }

  /**
   * This method returns the Connection to the Database
   *
   * @return
   */

  def setConnection(): Connection = {
    var conn: Connection = null
    val jdbcUrl = "jdbc:postgresql://localhost:5432/billdb"
    val username = "postgres"
    val password = "raghul"
    try {
      conn = DriverManager.getConnection(jdbcUrl, username, password)
      conn
    }
    catch {
      case _: SQLException => println("ERROR - set Connection ")
        null
    }
  }

  /**
   * This Method Set the Encryption for the new UserID
   *
   * @param cusId    Customer ID
   * @param password Password to Set Encryption
   */

  def setEncryption(cusId: Int, password: String): Unit = {
    alterTable("UPDATE billdbschema.credentials SET password='" +
      encrypt(password) + "' WHERE cusid=" + cusId)
  }

  /**
   * This Method Encrypt the password
   *
   * @param password Password to Encrypt
   * @return
   */
  def encrypt(password: String): String = {
    password.map(x => {
      if (x.equals('z')) 'a' else if (x.equals('Z')) 'A'
      else if (x.equals('9')) '0' else (x + 1).toChar
    })
  }

  /**
   * This Method Decrypt the Password
   *
   * @param password password to Decrypt
   * @return
   */

  def decrypt(password: String): String = {
    password.map(x => {
      if (x.equals('a')) 'z' else if (x.equals('A')) 'Z' else if (x.equals('0')) '9'
      else (x - 1).toChar
    })
  }

  /**
   * This Method Echo the Character
   *
   * @param password password to echo
   * @return
   */

  def echo(password: String): String = {
    password.map(_ => "*").toString()
  }

  /**
   * This Method perform Login operation
   */
  def login(): Unit = {
    var userID: String = ""
    var passWord: String = ""
    println(Main.getDate)
    println("Online Billing Application")
    println("===========LOGIN============")

    println("=>Enter Your UserID:")
    userID = scala.io.StdIn.readLine()
    println("=>Enter Your Password:")
    passWord = scala.io.StdIn.readLine()
    println("============********===========")
    val cur = {
      if (authentication(userID, passWord).equals("")) {
        authentication(userID, encrypt(passWord)).toInt
      }
      else
        authentication(userID, passWord).toInt
    }
    if (isPresent(cur)) {
      println("please Enter Valid Credentials")
    }
    else {
      if (isHistoryPresent(cur)) {
        setEncryption(cur, passWord)
      }
      if (findRole(cur).equalsIgnoreCase("Customer")) {
        new Customer(cur).performCusOperation()
      }
      else if (!isPresent(cur)) {
        new Admin(cur).performAdminOperation()
      }
    }
  }

  /**
   * This Method Authenticates the User by UserName Password
   *
   * @param userName UserName
   * @param passWord Password
   * @return
   */
  def authentication(userName: String, passWord: String): String = {
    val t = getTable("SELECT cusid from billdbschema.credentials " +
      "WHERE username='" + userName + "' and password='" +
      passWord + "'")
    if (t.isEmpty) {
      ""
    }
    else {
      t.head.filter(c => c != ',').trim
    }

  }

  /**
   * This Method used to find the Roll of the User CusID
   *
   * @param cusID Customer ID
   * @return
   */
  def findRole(cusID: Int): String = {
    getTable("SELECT role FROM billdbschema.credentials " +
      "WHERE cusid=" + cusID).head.filter(c => c != ',').trim
  }

  /**
   * This Method Returns the System Date
   *
   * @return
   */
  def getDate: String = {
    val ldt: LocalDateTime = LocalDateTime.now()
    DateTimeFormatter.ofPattern("yyyy-MM-dd",
      Locale.ENGLISH).format(ldt)
  }

  /**
   * This the Util Method which can get specified table from
   * BilldbSchema.credentials for the Passed Query
   *
   * @param Query Query to Get Table
   * @return
   */

  def getTable(Query: String): List[String] = {
    val conn = setConnection()
    val temp = new ListBuffer[String]()
    val temp1 = new ListBuffer[String]()
    try {
      var res = ""

      val resSet = conn.createStatement().executeQuery(Query)
      var row = 0
      val resMeta = resSet.getMetaData

      if (conn == null) {
        println("ERROR: While Get Table")
      }
      while (resSet.next()) {
        row += 1
        for (i <- 1.to(resMeta.getColumnCount)) {
          res += resSet.getString(i) + ","
        }
        temp += res
        res = ""
      }
      temp.toList
    }
    catch {
      case _: SQLException =>
        temp1.toList
    }
    finally {
      conn.close()
    }

  }

  /**
   * This the Util Method which can perform a Update Operation in
   * table from BilldbSchema.credentials for the Passed Query
   *
   * @param Query Query to Alter Table
   * @return
   */

  def alterTable(Query: String): Boolean = {
    val conn = setConnection()
      conn.createStatement().execute("BEGIN")
      conn.createStatement().execute("SAVEPOINT check1")
      try {
        conn.createStatement().executeUpdate(Query)
        true
      }
    catch
    {
      case _: SQLException => conn.createStatement().execute("ROLLBACK TO SAVEPOINT check1")
        false
    }
    finally
    {
      conn.createStatement().execute("COMMIT")
      conn.close()
    }
  }
  /**
   * This Method Returns false if the CusID is Present else return true
   *
   * @param Id customer ID
   * @return
   */
  def isPresent(Id: Int): Boolean = {
    getTable("Select cusid from billdbschema.credentials " +
      "where cusid=" + Id).isEmpty
  }

  /**
   * This Method Returns false if the ProductID is Present in Product
   * Table else return true
   *
   * @param Id Product ID
   * @return
   */
  def isProductPresent(Id: Int): Boolean = {
    getTable("Select pid from billdbschema.input2 " +
      "where pid=" + Id).isEmpty
  }

  /**
   * This Method Returns false if the CusID is Present in History Table
   * else return true
   *
   * @param Id Customer ID
   * @return
   */
  def isHistoryPresent(Id: Int): Boolean = {
    getTable("Select distinct cusid from billdbschema.history " +
      "where cusid=" + Id).isEmpty
  }
}
