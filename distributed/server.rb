require 'rubygems'

require 'sinatra'
require 'sinatra/async'
require 'sinatra/base'

require 'json'
require 'uri'
require 'net/http'

require_relative 'worker.rb'
require_relative 'problem.rb'


class Server < Sinatra::Base

  # Server

  $available_workers = []

  $worker_limit = 2

  # problem that is currently being worked on
  $current_problem = nil

  $workers = []

  get '/status' do
  <<eos 
  <h2>Active workers:</h2>
  <pre>#{$workers}</pre>
  <h2>Current problem:</h2>
  <pre>#{$current_problem}</pre>
eos
  end


  post '/register' do
    content_type :json

    # we have enough workers, sorry :)
    if $workers.size >= $worker_limit
      halt 429
    end

    $current_problem = get_problem if $current_problem == nil

    train_request = JSON.parse request.body.read
    id = train_request["workerID"]

    puts "Worker #{id} is registering"

    worker = find_worker(id)

    if worker == nil
      job = $current_problem.split($worker_limit, $workers.size)
      worker = Worker.new(id, job)
      $workers << worker
    end
    
    worker.job.to_json
  end

  post '/eval' do
    content_type :json

    # wait until all clients registered
    if $workers.size < $worker_limit
      halt 429
    end

    eval_request = JSON.parse request.body.read
    id, worker_id = eval_request["id"], eval_request["workerID"]

    puts "Worker #{worker_id} is performing an eval with #{eval_request}"

    worker = find_worker(worker_id)

    data = nil

    # TODO other error code?
    if $current_problem == nil or worker == nil
      halt 404

    # already solved
    elsif id != $current_problem.id
      halt 412

    else

      # we already have data points for this request count?
      # -> just use it
      if $current_problem.data_points.size > worker.calls_to_eval
         data = $current_problem.data_points[worker.calls_to_eval]

      # there are no datapoints, yet? 
      # -> Do a request to M$
      else
        data = request_eval($current_problem.id, eval_request["arguments"])
        $current_problem.add_data_points(data)
      end

      worker.called_eval!

      {
        "status" => "ok",
        "arguments" => data["arguments"],
        "outputs" => data["outputs"]
      }.to_json
    end
  end

  post '/guess' do
    content_type :json

    guess = JSON.parse request.body.read

    puts "guessing #{guess}"

    # already solved
    if guess["id"] != $current_problem.id
      halt 412

    else
      result = request_guess(guess["id"], guess["program"])

      if result['status'] == "win"
        puts "We won! #{guess["id"]} #{guess["program"]}"
        $workers = []
        $current_problem = nil
      else

        puts "Wrong guess #{guess["id"]} #{guess["program"]}"
      end

      result.to_json
    end
  end

  private

  def find_worker(id)
    $workers.select {|worker| worker.id == id }.first
  end


  # communication with the real server

  def get_problem

    # production one

    # test one
=begin
{
      "id" => "9PIpu5iWYnSSPnO8wklvBPV7",
      "size" => 26,
      "operators" => ["and", "fold", "if0", "not", "or", "plus", "shr16", "shr4", "xor"],
      "challenge" => "(lambda (x_67997) (fold x_67997 (shr4 (xor (and (not (or x_67997 0)) (not (if0 (plus (xor x_67997 (shr16 x_67997)) 1) x_67997 1))) x_67997)) (lambda (x_67998 x_67999) (shr4 (plus x_67998 x_67999)))))"
    }
=end

    Problem.new(request_train)
  end

  def request_guess(id, program)
    resp = perform_request('guess', {
      id: id,
      program: program
    })

    if resp.code == '200'
      JSON.parse(resp.body)

    else
      puts data["message"]
      halt 429
    end
  end


  # returns
  def request_eval(id, arguments)

    resp = perform_request('eval', {
      id: id,
      arguments: arguments
    })


    if resp.code == '200'
      data = JSON.parse(resp.body)
      
      if data["status"] == "ok"
        return {
          "arguments" => arguments,
          "outputs" => data["outputs"]
        }

      # TODO not sure with this
      else
        puts data["message"]
        halt 429
      end


    # currently just break!
    else
      halt resp.code.to_i
    end
  end


  def request_train
    resp = perform_request('train')

    if resp.code == '200'
      JSON.parse(resp.body)

    # currently just break!
    else
      halt resp.code.to_i
    end    

  end

  def secret
    '02768XDijvjky5OOedNdAnRxokV6hSA8aaFT1doK'
  end

  def perform_request(path, payload = nil)
    puts "Communicating with real server! /#{path} \n#{payload.inspect}"
    payload = JSON.generate(payload) unless payload == nil
    http = Net::HTTP.new(uri.host, uri.port)    
    http.post(buildPath(path), payload)
  end

  def uri
    URI('http://icfpc2013.cloudapp.net')
  end

  def buildPath(path)
    "/#{path}?auth=#{secret}vpsH1H"
  end
end