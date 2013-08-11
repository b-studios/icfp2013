require 'json'
require_relative 'worker.rb'

class Problem

  attr_accessor :time_is_running
  # data_points is an array of hashs: { arguments: [], outputs: [] }
  attr_reader :problem_desc, :workers, :pieces, :data_points

  def initialize(prob)
    @problem_desc = prob
    @workers = []
    @pieces = nil
    @data_points = []
  end

  def queueUpWorker(worker)
    workers << worker
    workers.uniq!
  end

  def split(total, i)
    # calc_pieces(pieceCount)
    @problem_desc.clone.merge({
      workerNumber: i + 1,
      totalWorkers: total
    })
  end

  def add_data_points(data)
    @data_points << data
  end

  def id
    @problem_desc["id"]
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
    "Problem #{@problem_desc["id"]} data-points:#{@data_points}"
  end

end