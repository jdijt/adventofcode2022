package eu.derfniw.aoc2022

class Exercise07Test extends AocTestSuite:

  val sample = """|$ cd /
                  |$ ls
                  |dir a
                  |14848514 b.txt
                  |8504156 c.dat
                  |dir d
                  |$ cd a
                  |$ ls
                  |dir e
                  |29116 f
                  |2557 g
                  |62596 h.lst
                  |$ cd e
                  |$ ls
                  |584 i
                  |$ cd ..
                  |$ cd ..
                  |$ cd d
                  |$ ls
                  |4060174 j
                  |8033020 d.log
                  |5626152 d.ext
                  |7214296 k""".stripMargin

  testStringSample("Part one on sample input", sample, ex07.run01, 95437)
  testStringSample("Part two on sample input", sample, ex07.run02, 24933642)
end Exercise07Test
