class Task
  attr_accessor :problem, :total, :part, :successful_evals

  def initialize(worker, problem, part, total)
    @assigned_worker, @problem, @part, @total = worker, problem, part, total
    @successful_evals = 0
  end

  # TODO
  def reassign(worker)
    @assigned_worker = worker
    @successful_evals = 0
  end

  def unassign
    @assigned_worker = nil
  end

  def assigned?
    @assigned_worker != nil
  end

  def to_json
    @problem.problem_desc.clone.merge({
      workerNumber: part,
      totalWorkers: total
    }).to_json
  end

  def called_eval!
    @successful_evals += 1
  end

  #TODO
  def to_s
    "Task (#{part}/#{total})"
  end
end