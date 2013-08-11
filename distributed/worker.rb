class Worker

  attr_reader :id, :job, :calls_to_eval

  def initialize(id, job)
    @id = id
    @job = job
    @calls_to_eval = 0
  end

  def to_s
    "Worker id:#{@id} calls_to_eval:#{@calls_to_eval}"
  end

  def called_eval!
    @calls_to_eval = @calls_to_eval + 1
  end

end