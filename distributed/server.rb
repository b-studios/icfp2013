require 'rubygems'

require 'sinatra'
require 'sinatra/base'

require 'json'
require 'uri'
require 'net/http'

require_relative 'worker.rb'
require_relative 'problem.rb'


class Server < Sinatra::Base

  PRODUCTION = true

  def initialize
    @real_problems = request_problems.select { |p| ! (p["solved"] == true) }.sort { |a, b|
      a["size"] - b["size"]
    }.select {|prob| 
      prob["size"] < 5
    }
  end

  
  # problem that is currently being worked on
  $current_problem = nil

  $old_problems = []

  $workers = []

  get '/status' do
  <<eos 
  #{$real_problems}
  <pre>Expected workers: </pre>
  <h2>Active workers:</h2>
  <pre>#{$workers}</pre>
  <h2>Current problem:</h2>
  <pre>#{$current_problem}</pre>
  <h2>Old problems:</h2>
  <ul>#{ $old_problems.map {|p| "<li><pre>#{p.id} #{p.size}</pre></li>" }.join("\n") }</ul>
  <script>window.onload = function() { window.setInterval(function() { window.location.reload() }, 1500) }</script>
eos
  end


  # TODO if we kill a client and a new one registers we have to
  # distribute the work anew - but correctly
  get '/stay_alive/:id' do |id|

    # now we have to figure out when to kill the client
    worker = find_worker(id)

    halt 410 if worker == nil or worker.kill? # TODO problem.timeout?
  end


  post '/register' do

    content_type :json

    reg_request = JSON.parse request.body.read
    worker = find_or_create_worker reg_request["workerID"]

    puts "Worker #{reg_request["workerID"]} is registering"

    if worker.assigned? and $current_problem != worker.task.problem
      worker.unassign
    end

    return worker.task.to_json if worker.assigned?

    # create a new problem ! NETWORK ACTION HERE !
    $current_problem = get_problem if no_current_problem

    # we have enough workers, sorry :)
    halt 423 if $current_problem.enough_workers?

    task = $current_problem.assign_task_to worker
    
    task.to_json
  end

  post '/eval' do
    content_type :json
    
    eval_request = JSON.parse request.body.read
    worker = find_worker eval_request["workerID"]

    check_preconditions_for worker

    halt 420 unless $current_problem.enough_workers?
    halt 412 unless $current_problem.id == eval_request["id"]

    puts "Worker #{ worker } is performing an eval with #{ eval_request }"

    data = nil

    if $current_problem.actual_calls_to_eval > worker.task.successful_evals
      data = $current_problem.cached_eval(worker.task.successful_evals)

    # there are no datapoints, yet? -> Do a request to M$
    else
      data = request_eval(eval_request) do |code, msg|

        case code
        when 410
          $current_problem.failed!
          worker.unassign

        when 412
          solved_problem!
          worker.unassign

        when 429
          puts "Too many requests"

        else
          puts "Something terribly happend, code: #{code}"
        end
          
        halt code
      end

      $current_problem.add_data_points(data)
    end

    worker.task.called_eval!

    data.to_json
  end

  post '/guess' do
    content_type :json

    guess_request = JSON.parse request.body.read
    worker = find_worker guess_request["workerID"]

    check_preconditions_for worker

    puts "guessing #{guess_request}"

    
    result = request_guess(guess_request) do |code|

      case code
      when 410
        $current_problem.failed!
        worker.unassign

      when 412
        solved_problem!
        worker.unassign

      when 429
        puts "Too many requests"

      else
        puts "Something terribly happend, code: #{code} (or it's a 429)"

      end

      halt code
    end

    if result['status'] == "win"
      puts "We won! #{guess_request["id"]} #{guess_request["program"]}"

      solved_problem!
      worker.unassign

      $current_problem = nil

    else
      puts "Wrong guess #{guess_request["id"]} #{guess_request["program"]}"
    end

    result.to_json  
  end

  private

  def solved_problem!
    $current_problem.solved!
    $old_problems << $current_problem
    $old_problems.uniq!
  end

  def check_preconditions_for(worker)
    # we abuse this for already solved and failed
    halt 412 if no_current_problem

    halt 666 if worker == nil

    halt 410 if $current_problem.failed?
    halt 412 if $current_problem.solved?
    halt 412 if worker.task.problem.id != $current_problem.id # for sanity - already solved
  end

  def no_current_problem
    $current_problem == nil
  end

  def find_worker(id)
    $workers.select {|worker| worker.id == id }.first
  end

  def find_or_create_worker(id)
    w = find_worker id

    if w == nil
      w = Worker.new id
      $workers << w
    end
    w
  end

  # communication with the real server


  def get_problem

    # production one
    if PRODUCTION
      prob = @real_problems.unshift
      halt 404 if prob == nil

      Problem.new(prob)

    # test one
=begin
Problem.new({
  "id" => "pGRnFfwszdPRNzzDQAJxw16h",
  "size" => 12,
  "operators" => ["and","if0","or","shr4","xor"],
  "challenge" => "(lambda (x_12795) (xor (if0 (or (and (shr4 0) x_12795) x_12795) x_12795 1) x_12795))"
})
=end

    else
      Problem.new(request_train)
    end
  end


  # returns
  def request_eval(eval_request, &error_callback)

    resp = perform_request('eval', eval_request)

    if resp.code == '200'
      data = JSON.parse(resp.body)
      
      if data["status"] == "ok"
        data["arguments"] = eval_request["arguments"]
        return data

      # TODO not sure with this
      else
        error_callback.call(400, data["message"])
      end

    else 
        error_callback.call(resp.code.to_i, "something went wrong")
    end
  end


  def request_guess(guess_request, &error_callback)
    resp = perform_request('guess', guess_request)

    if resp.code == '200'
      JSON.parse(resp.body)

    else
      error_callback.call(resp.code.to_i, "something went wrong - while guessing")
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

  def request_problems
    resp = perform_request('myproblems')

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