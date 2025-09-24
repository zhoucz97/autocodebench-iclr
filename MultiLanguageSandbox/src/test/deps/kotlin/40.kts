
/**
    * Find the author who has published the most books and return the author's initial along with the IDs of books they've contributed to.
    * The input is a list of pairs, where each pair consists of a book ID (Int) and a string of unique uppercase letters representing authors.
    * The function returns a pair with the most published author's initial (Char) and a list of book IDs (List<Int>) they've contributed to.
    Example:

    >>> findMostPublishedAuthor(listOf(Pair(410, "GPKCV"), Pair(567, "SPIM"), Pair(822, "YSHDLPM")))
        Pair('P', listOf(410, 567, 822))
**/


fun findMostPublishedAuthor(books: List<Pair<Int, String>>): Pair<Char, List<Int>> 
{
    val initialToBookIds = mutableMapOf<Char, MutableList<Int>>()
    val initialCounts = mutableMapOf<Char, Int>()

    for ((bookId, authors) in books) {
        for (author in authors) {
            val initial = author
            initialToBookIds.getOrPut(initial) { mutableListOf() }.add(bookId)
            initialCounts[initial] = initialCounts.getOrDefault(initial, 0) + 1
        }
    }

    if (initialCounts.isEmpty()) {
        return Pair(' ', emptyList())
    }

    val maxInitial = initialCounts.maxByOrNull { it.value }?.key ?: ' '
    val bookIds = initialToBookIds[maxInitial] ?: emptyList()

    return Pair(maxInitial, bookIds)
}
fun main() {
    check(findMostPublishedAuthor(listOf(Pair(307, "F"), Pair(895, "HF"))) == Pair('F', listOf(307,895)))
    check(findMostPublishedAuthor(listOf(Pair(307, "F"), Pair(895, "H"), Pair(410, "GPKCV"), Pair(567, "SPIM"), Pair(822, "YSHDLPM"), Pair(834, "BXPRD"), Pair(872, "LJU"), Pair(791, "BPJWIA"), Pair(580, "AGMVY"), Pair(619, "NAFL"), Pair(233, "PDJWXK"))) == Pair('P', listOf(410,567,822,834,791,233)))

 
}
main()