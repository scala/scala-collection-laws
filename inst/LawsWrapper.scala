package laws

// This exists only to give SBT a run target unique for this project.
object LawsWrapper {
  def main(args: Array[String]) {
    // Looking things up by reflection tends to swallow errors.  Do it explicitly here to make sure we see load errors.
    println("Found Instances with " + laws.Instances.mapped.size + " mappings.")
    Laws.main(args)
  }
}
