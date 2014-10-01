package laws

// This exists only to give SBT a run target unique for this project.
object LawsWrapper {
  def main(args: Array[String]) {
    Laws.main(args)
  }
}
