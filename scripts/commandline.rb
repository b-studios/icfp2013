#!/usr/bin/ruby

require "rubygems"
require "thor"
require "uri"
require "net/http"
require "json"

class IcfpCli < Thor



  # should be cached later on
  desc "list", "lists our problems"
  def list()
    puts performRequest('myproblems')
  end

  desc "eval PROGRAM ARGS...", "evaluates a given PROGRAM"
  def eval(program, *args)

    puts performRequest('eval', {
      :program => program,
      :arguments => args
    })
  end

  desc "evalid ID ARGS...", "evaluates the secret program (with id ID) - USE WITH CARE!"
  def evalid(id, *args)

    puts performRequest('eval', {
      :id => id,
      :arguments => args
    })
  end

  desc "guess ID PROGRAM", "guesses the solution for the program with id ID - USE WITH CARE!"
  def guess(id, program)

    puts performRequest('guess', {
      :id => id,
      :program => program
    })
  end

  private

  def secret
    '02768XDijvjky5OOedNdAnRxokV6hSA8aaFT1doK'
  end

  def performRequest(path, payload = nil)
    payload = JSON.generate(payload) unless payload == nil

    puts payload
    http = Net::HTTP.new(uri.host, uri.port)    
    handleResponse http.post(buildPath(path), payload)
  end

  def handleResponse(response)

    if response.code == '200'
      JSON.pretty_generate JSON.parse(response.body)
    else
      response.msg
    end

  end

  def uri
    URI('http://icfpc2013.cloudapp.net')
  end
0x00000000000001
  def buildPath(path)
    "/#{path}?auth=#{secret}vpsH1H"
  end

end
 
IcfpCli.start(ARGV)
