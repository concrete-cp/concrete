package concrete.constraint.semantic.energy

case class CumulativeArguments(
                                doRuntimeComparison: Boolean = false,
                                useCache: Boolean = false,
                                useVirtualInitialisation: Boolean = false,
                                restrictBinarySearch: Boolean = false,
                                virtualCache: VirtualInitialisationCache,
                                numberOfPasses: Int = 4
                              )
