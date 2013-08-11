require 'strscan'

class StatsParser

  attr_reader :stats

  LPAREN   = /[(]/
  RPAREN   = /[)]/
  NAME     = /\w+/
  LAMBDA   = /lambda/
  WS       = /\s+/
  LITERALS = /[01]/
  VAR      = /x_\d+/

  KEYS     = ["not", "shl1", "shr1", "shr4", "shr16", "and", "or", "xor", "plus" , "one", "zero", "fold", "if0", "input", "acc", "byte"]

  def initialize(prog)
    @scanner = StringScanner.new prog
    @stats = {}
    @stack = []
    # maps id to var type
    @vars = {}

    @literals = {
      "0" => "zero",
      "1" => "one"
    }

    init_stats
    parseToplevelLambda
  end

  def +(other)

    (KEYS + ["toplevel"]).map do |group|
      KEYS.each do |key|
        @stats[group]["general"][key] += other.stats[group]["general"][key]
        @stats[group]["direct"][key] += other.stats[group]["direct"][key]
      end
    end

    self
  end

  private

  def parseToplevelLambda
    consume LPAREN
    consume LAMBDA
    skipWS
    consume LPAREN
    @vars[consume VAR] = "input"
    consume RPAREN

    @stack.push 'toplevel'

    parseExp

    @stack.pop

    consume RPAREN
  end

  def parseExp

    skipWS

    if matches? LITERALS
      parseLiteral

    elsif matches? VAR
      parseVar

    elsif matches? LPAREN
      parseSExp

    else
      return
    end
  end

  def parseSExp
    consume LPAREN

    op = consume NAME
    skipWS

    add_to_stats op
    @stack.push op
    if op == "fold"
      parseFoldBody
    else
      while not matches? RPAREN
        parseExp
      end
    end
    @stack.pop

    consume RPAREN
  end

  def parseFoldBody()
    skipWS
    first = parseExp
    skipWS
    second = parseExp
    skipWS
    consume LPAREN
    consume LAMBDA
    skipWS

    consume LPAREN
    @vars[consume VAR] = "byte"
    skipWS
    @vars[consume VAR] = "acc"
    consume RPAREN

    body = parseExp

    consume RPAREN
  end

  def parseVar()
    add_to_stats @vars[consume VAR]
  end

  def parseLiteral()
    add_to_stats @literals[consume LITERALS]
  end

  def add_to_stats(token)

    @stack.uniq.each do |parent|
      count_general parent, token
    end

    count_direct @stack.last, token
  end

  def count_general(group, key)
    @stats[group]["general"][key] += 1
  end

  # immediate children of group
  def count_direct(group, key)
    @stats[group]["direct"][key] += 1
  end

  def init_stats
    (KEYS + ["toplevel"]).each do |group|
      @stats[group] = { "general" => {}, "direct" => {} }
      KEYS.each do |key|
        @stats[group]["general"][key] = 0
        @stats[group]["direct"][key]  = 0
      end
    end
  end

  def skipWS
    consume WS
  end

  def consume(token)
    @scanner.scan token
  end

  def matches?(token)
    @scanner.match? token
  end
end