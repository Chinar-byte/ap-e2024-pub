#let content_settings(doc) = {
	show par: set block(spacing: 1.5em)
	show heading: it => {
		set block(above: 1.4em, below: 1em)
        it
	}

	set page(
       	paper: "a4",
        margin: (
            top: 3cm,
            bottom: 4cm,
            left: 3.2cm,
            right: 3.2cm
        ),
		numbering: "1 / 1",
	)
	
	set par(
	   	leading: 1em,
	   	first-line-indent: 1em,
	   	justify: true,
	)

	set enum(
		indent: 2em,
		numbering: "1.",
	)

	set list(
		indent: 1em,
	)

	set math.mat(
		delim: "[",
	)

	set math.vec(
		delim: "[",
	)

    set quote(block: true)

	context counter(page).update(1)

	set heading(
	   	numbering: "1.1",
	   	bookmarked: true,
	)

    set text(font: "EB Garamond", size: 12pt, lang: "en")

    set math.equation(
        numbering: "(1)",
    )

    doc
}

#let table_of_contents(
	name: "Contents"
) = {
	show outline.entry.where(
		level: 1
	): it => {
		v(2em, weak: true)
		strong(it)
	}

	outline(
		fill: box(width: 95%, repeat([#h(0.5em).])),
		indent: auto,
		title: name,
	)
}

#let make_title(title, author) = [
    #v(3em)
    #align(center)[
        #text(size: 24pt)[
            * #title *
        ]
    ]

    #v(1em)

    #align(center)[
        #text(author, size: 16pt)
    ]
]

#let ask(question) = block[
    * #question *
]

#let assignment(
    title: none,
    author: ("Carlo Rosso rkm957",),
    doc,
) = {
    show: body => content_settings(body)

    set document(
        title: title,
        author: author,
        date: datetime.today(),    
    )

    make_title(title, author.join(", "))

    v(1cm)

    table_of_contents()

    v(1cm)

    doc
}


#show: doc => assignment(
		title: [ Advanced Programming \ Home Assignment 6 ],
        author: ("Carlo Rosso", "Chinar Shah"),
		doc
)

= Intro

Overall this assignment was very top heavy, with the first exercise being the most time consuming to implement. This was mostly because it took some time to understand the context of the question and the overall assignment before we had to implement it. But after the first exercise was done - the rest was fairly fast and easy. However, in saying that it was difficult to ascertain what we needed to implement because there were so many moving parts. The biggest uncertainty was if we had to implement the whole state monad for the workers. Reflecting back on the work, we could probably make the SPCM more complex so that it can handle everything, but it would be much more messier and convoluted.


= Adding Workers

We implemented `workerAdd` with the following steps:
+ Implement `WorkerM` monad which represents a running worker with its state
  - We emulated `SPCM` monad because they are both stateful monads and they both run a server process to allow communication
+ Add `MsgWorkerAdd` to `SPCMsg` to allow adding workers with the signature
  `MsgWorkerAdd WorkerName (ReplyChan (Either String Worker))`
+ We implemented `workerAdd`

We also need to define a new message for testing purpose:
+ Add `spcWorkers` to the `SPCState` to keep track of the workers
+ Implement `workerAssignJob` to assign a job to a worker
+ Add `MsgJobDone` to `SPCMsg` to allow workers to notify the SPC when a job is done
+ The same thing goes also for `Worker` therefore we added the messages
    - `WorkerJobNew` 
    - `WorkerJobDone` 
    to `WorkerMsg` and added the handlers for those in the `workerHandle` function
+ Implement the handler for `MsgWorkerAdd`, which is reported above
+ Implement the handler for `MsgJobDone`, which is reported above
+ Implement `schedule` to assign jobs to workers if possible
+ Add `spcWaiters` to the `SPCState` to keep track of the waiters for jobs 
+ Implement handler for `MsgJobWait` to add a waiter to the list of waiters
+ Implement `jobWait` to wait for a job to finish
+ Finally, we added `spcChan` to the `SPCState`, which is the channel used by
  the SPC server to receive messages

Whilst difficult to implement, the solution is functional and covered in the tests of 'adding job'

= Job Cancellation

Since we already defined the monad earlier, implementing the cancellation
was very easy, straightforward and similar to the exercise. 

Note that if the job is pending, we do not need to interact with any worker,
otherwise we need to find the worker that is running the job and tell it to
stop. We implemented it, by sending a message to all the workers. The workers
have the job id in their state, so they can check if they are running the job
with that id, if that is the case they will cancel the job. Overall, the solution for this was functional and tested with the test 'canceling job'.

= Timeouts

We implemented both the solutions, because we wanted to see the difference, below. Both solutions were functionally correct and were working. From this implementation we have a preferance for the workers to manage the timeouts because it simplifies the SPC, making it more lightweight. Here the SPC can focus on managing the workers while the workers are only focused on the jobs. Hence this method is more modular and clean. 

== Managed by SPC

```haskell
checkTimeouts :: SPCM ()
checkTimeouts = do
  now <- io getSeconds
  state <- get
  forM_ (spcJobsRunning state) $ \(jobid, deadline) ->
    when (now >= deadline) $ do
      jobDone jobid DoneTimeout
      io $ send (spcChan state) $ MsgJobCancel jobid
```

The core of the implementation is `checkTimeouts`:
+ Update the state of SPCM so that `spcJobsRunning` contains the job id and the deadline of each job (we do not need the job itself anymore)
+ On the server start we add a thread that sends a `MsgTick` every second just like in the exercise
+ At every tick we call `checkTimeouts` that checks whether a job has exceeded the deadline and it cancels the job

We confirm the functionality in the 'timeout' test. 

= Exception

This was the simplest assignment task as we can just emulate the exercise code. Hence that can attest to its functionality. 

= Removing workers 

This was also fairly fast as we had the context of the assignment and the structure of the code understood. 
This was done by the following code - which is functionally correct.

```haskell 
haskell
    MsgWorkerStop wname -> do
      state <- get
      case lookup wname $ spcWorkers state of
        Just worker -> do
          io $ workerStop worker
          put $ state {spcWorkers = removeAssoc wname $ spcWorkers state}
        Nothing -> pure ()
```

= Questions

#ask[
What happens when a job is enqueued in an SPC instance with no workers, and a worker is added later? Which of your tests demonstrate this?
]

When a job is enqueued in an SPC instance with no workers, it will be placed in the pending jobs queue. The job will remain in this state until a worker is added to the SPC instance, at which point the SPC will assign the pending job to the newly added worker.

The relevant test case that demonstrates this behavior is 'adding worker'

#ask[
Did you decide to implement timeouts centrally in SPC, or decentrally in the
worker threads? What are the pros and cons of your approach? Is there any
observable effect?
]

== Managed by SPC

Con's
- Not the simplest solution 

Pro's:
- More modular
- Simplify the worker and make the SPC more complex

#ask[
Which of your tests verify that if a worker executes a job that is cancelled, times out, or crashes, then that worker can still be used to execute further jobs?
]

The tests that verify if a worker executes a job that is cancelled are found in the test file under 

+ 'canceling job'
+ 'timeout'
+ 'crash'

All of these tests confirm that the worker can still be used to execute the tests

#ask[
If a worker thread were somehow to crash (either due to a bug in the worker thread logic, or because a scoundrel killThreads it), how would that impact the functionality of the rest of SPC?
]

The impact of a worker crash on the SPC depends on how the timer is implemented. If the timer is managed by the worker itself, the SPC will remain unaware of the worker's crash until it attempts to communicate with the worker. Consequently, any job being executed by the crashed worker will remain incomplete and stuck in the "Running" state, which could lead to resource contention and hinder overall system performance.

On the other hand, if the timer is managed by the SPC, it will detect when a job exceeds its deadline. In this scenario, the SPC will automatically cancel the job, moving it into the spcJobsDone list with a status of DoneTimeout. This allows the SPC to maintain an accurate representation of job statuses and ensures that resources can be reallocated efficiently. Overall, managing the timer at the SPC level enhances system resilience by enabling timely job management and reducing the potential for unmonitored job failures.