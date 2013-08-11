require 'time'

class Worker

  TIMEOUT = 300

  attr_reader :id, :task

  def initialize(id)
    @id = id
    @last_contact = Time.new
  end

  def to_s
    "Worker id:#{@id} task: #{task.to_s}"
  end

  def tick
    @last_contact = Time.new
  end

  def timeout?
    Time.new - @last_contact > TIMOUT
  end

  def kill?
    @task.problem.running?
  end

  def assign_task(task)
    @task = task
  end

  def assigned?
    @task != nil # and @task.calls_to_eval == 0
  end

  def unassign
    @task.unassign
    @task = nil
  end

  def free?
    @task == nil
  end

  def busy?
    assigned? and @task.successful_evals > 0
  end

end