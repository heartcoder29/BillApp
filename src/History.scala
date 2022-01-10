object History {
  /**
   * This Method addRecord to History Table
   *
   * @param cusid   Customer ID
   * @param records Record for the Session
   */
  def addRecord(cusid: Int, records: List[String]): Unit = {
    var temp = "'logged in',"
    for (i <- records) temp += (i + ",")
    temp += "'logged out'"
    Main.alterTable("insert into billdbschema.history values(" +
      "ARRAY [" + temp + "],'" + Main.getDate + "'," + cusid + ")")
  }

  /**
   * This Method Display the History Records fo the Customer for
   * Each Session
   *
   * @param date  Date of the History Performed
   * @param array Array of  Records
   */
  def printHistory(date: String, array: Array[String]): Unit = {
    printf("%-20s%-60s\n", date, "==================")
    for (i <- array) printf("%-20s%-60s\n", " -------------------------- ", i)
  }

  /**
   * This Method Splits the Each Transaction for Each Session
   *
   * @param strings String Of Transaction
   * @return
   */
  def splitTransaction(strings: String): Array[String] = {
    strings.filter(x => x != '}').filter(x => x != '{').split(",+")
  }
}