require 'json'
require_relative 'worker.rb'
require_relative 'task.rb'

class Problem

  WORKERS_NEEDED = 17

  attr_accessor :tasks

  # data_points is an array of hashs: { arguments: [], outputs: [] }
  attr_reader :problem_desc, :workers, :pieces, :data_points, :status

  def initialize(prob)
    @problem_desc = prob
    @workers = []
    @tasks = []
    @pieces = nil
    @data_points = []
    @status = :running
  end

  def add_data_points(data)
    @data_points << data
  end

  def id
    @problem_desc["id"]
  end

  def size
    @problem_desc["size"]
  end

  def solved?
    @status == :solved
  end

  def solved!
    @status = :solved
  end

  def failed?
    @status == :failed
  end

  def failed!
    @status = :failed
  end

  def running?
    @status == :running
  end

  def actual_calls_to_eval
    @data_points.size
  end

  def cached_eval(index)
    @data_points[index]
  end

  def assign_task_to(worker)
    task = @tasks.select {|t| not t.assigned? }.first

    if task != nil
      task.reassign worker

    else
      task = Task.new worker, self, number_assigned_tasks + 1, WORKERS_NEEDED
      @tasks << task
    end

    worker.assign_task task
    return task
  end

  def enough_workers?
    number_assigned_tasks >= WORKERS_NEEDED
  end

  def number_assigned_tasks
    @tasks.select {|t| t.assigned? }.size
  end


  private

  # TODO
  def calc_pieces(pieceCount)
    if @pieces == nil
      @pieces = @problem_desc["operators"].map { |op|
        [op]
      }
    end
  end

  def to_s
    "Problem #{@problem_desc["id"]} status: #{status} data-points:#{@data_points}"
  end

end
