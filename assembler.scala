//> using toolkit 0.6.0

extension (i: Int)
  def bin(len: Int): String =
    i.toBinaryString.reverse.padTo(len, '0').take(len).reverse

extension (s: String)
  def parseBinary: Int =
    s.reverse
      .padTo(32, s(0))
      .zipWithIndex
      .map((c, i) => c.toString.toInt << i)
      .sum

enum Typ:
  case R, I, B

object Typ:
  val RRange = 0 to Inst.And.ordinal
  val IRange = (RRange.last + 1) to Inst.Jalr.ordinal
  val BRange = (IRange.last + 1) to Inst.values.last.ordinal

  def apply(inst: Inst): Typ =
    if RRange contains inst.ordinal then Typ.R
    else if IRange contains inst.ordinal then Typ.I
    else if BRange contains inst.ordinal then Typ.B
    else throw new IllegalArgumentException("Illegal Instruction")

enum Inst:
  case Add, Sub, Sll, Slt, Sltu, Xor, Srl, Sra, Or, And, // R
    Addi, Slli, Slti, Sltiu, Xori, Srli, Srai, Ori, Andi, Jalr, // I
    Beq, Bne, Blt, Bge, Bltu, Bgeu // B

  lazy val typ: Typ = Typ(this)

  lazy val op: String =
    typ match
      case Typ.R => 51.bin(7)
      case Typ.I => if this == Jalr then 103.bin(7) else 19.bin(7)
      case Typ.B => 99.bin(7)

  lazy val funct3: String =
    this match
      case Add | Sub | Addi | Jalr | Beq => "000"
      case Sll | Slli | Bne              => "001"
      case Slt | Slti                    => "010"
      case Sltu | Sltiu                  => "011"
      case Xor | Xori | Blt              => "100"
      case Srl | Sra | Srli | Srai | Bge => "101"
      case Or | Ori | Bltu               => "110"
      case And | Andi | Bgeu             => "111"

  lazy val funct7: String =
    this match
      case Sub | Sra | Srai => "0100000"
      case _                => "0000000"

end Inst

object Inst:
  def apply(s: String): Inst =
    Inst.values.find(_.toString.toLowerCase == s.toLowerCase).get

def reg(s: String): Int = s.tail.toInt

def parseLine(line: String): (Inst, Int, Int, String) =
  line.strip match
    case s"$inst $arg1 $arg2 $tail" =>
      val arg3 = tail match
        case s"$arg3#$comment" => arg3
        case arg3              => arg3
      (Inst(inst.strip), reg(arg1.strip), reg(arg2.strip), arg3.strip)
    case _ =>
      throw new IllegalArgumentException(
        s"The line could not be parsed:\n    $line"
      )

def parseArg3(
    lookupLabel: Map[String, Int]
)(
    instWithIndex: ((Inst, Int, Int, String), Int)
): (Inst, Int, Int, Int) =
  val ((inst, arg1, arg2, arg3), index) = instWithIndex
  val typ = Typ(inst)
  val newArg3 = typ match
    case Typ.R =>
      reg(arg3)
    case Typ.I =>
      arg3.toInt
    case Typ.B =>
      val line = lookupLabel(arg3)
      (line - index) * 4
  (inst, arg1, arg2, newArg3)

def toHex(instTuple: (Inst, Int, Int, Int)): String =
  val (inst, arg1, arg2, arg3) = instTuple
  val funct7 = inst.funct7
  val funct3 = inst.funct3
  val op = inst.op

  val bin = inst.typ match
    case Typ.R =>
      val rd = arg1.bin(5)
      val rs1 = arg2.bin(5)
      val rs2 = arg3.bin(5)
      s"$funct7$rs2$rs1$funct3$rd$op"
    case Typ.I =>
      val rd = arg1.bin(5)
      val rs1 = arg2.bin(5)
      val imm = inst match
        case Inst.Slli | Inst.Srli | Inst.Srai => // unsigned
          funct7 + arg3.bin(5)
        case _ =>
          arg3.bin(12)
      s"$imm$rs1$funct3$rd$op"
    case Typ.B =>
      val rs1 = arg1.bin(5)
      val rs2 = arg2.bin(5)
      val label = arg3.bin(12).reverse
      s"${label(11)}${label.slice(4, 10).reverse}$rs2$rs1$funct3${label.slice(0, 4).reverse}${label(10)}$op"

  assert(bin.size == 32, s"The instruction is not 32 bits:\n    $instTuple")
  bin.parseBinary.toHexString.reverse.padTo(8, '0').reverse
end toHex

def parseProgram(lines: IndexedSeq[String]): String =
  val (lookupLabel, newLines) =
    lines.indices.foldLeft(
      (Map.empty[String, Int], lines.map(_.replaceAll(",", "")))
    ): (acc, i) =>
      val (lookup, innerLines) = acc
      val offset = lines.size - innerLines.size
      val index = i - offset
      innerLines(index).strip match
        case s"$label:" =>
          (
            lookup + (label.strip -> (index + 1)),
            innerLines.patch(index, Nil, 1)
          )
        case s"$label:$tail" =>
          val updatedAcc = tail.strip match
            case s"#$_" =>
              innerLines.patch(index, Nil, 1)
            case _ =>
              innerLines.updated(index, tail)
          (lookup + (label.strip -> index), updatedAcc)
        case s"" | s"#$_" => (lookup, innerLines.patch(index, Nil, 1))
        case _   => acc

  newLines
    .map(parseLine)
    .zipWithIndex
    .map(parseArg3(lookupLabel))
    .map(toHex)
    .mkString("\n")

@main
def main(filePath: String): Unit =
  val wd = os.pwd
  val program = parseProgram(os.read.lines(wd / filePath))
  println(program)
