


Still working on it!

Been taking a bit longer to get the core worked out that I'd have liked, but life happens (eg my mom had cancer for a month this winter, though she's fine now, which is awesome. She didn't even need chemo or rad!!).
Also, I was original planning to NOT write my own linear algebra substrate, but I quickly realized all the current tools suck, and that I needed to come up with a better numerical substrate if I wanted to do better.
What do I mean by this? With all the numerical tools out there presently, there are none that address the following falsehood that many folks believe is true: "you can have high level tools that aren't extensible but are fast, or you can have low level tools that are extensible and fast.".

I want high level tools that are fast. I want high level tools that are fast AND extensible. I want it to be easy for the end user to add new matrix layouts (dense and structure, structured sparse, or general sparse) and have generic machinery for giving you all the general linear algebra machinery with only a handful of new lines of code per new fancy layout. I want to make it idiomatic and natural to write all your algorithms in a manner that gives you "level 3" quality memory locality. I want to make sure that for all but the most exotic of performance needs, you can write all your code in haskell. (and by exotic I mean, maybe adding some specialized code for certain fixed sized matrix blocks that fit l2 or l1, but really thats not most peoples real problems ).

Heres the key point in that ramble thats kinda a big deal: getting "level 3" quality memory locality for both sparse and dense linear algebra. I think I've "solved" that, though ultimately the reality of benchmarks will tell me over the coming few weeks if I have or not.

Likewise, I think I have a cute way of using all of this machinery to give a sane performance story for larger than ram on a single machine linear algebra! Theres going to be some inherent overhead to it, but it will work, and doing a cache oblivious optimal dense matrix multiply of 2 square 4gb+ ish sized matrices on a macbook air with 4gb of ram is going to be be a cute benchmark where no other lib will be able to do out of the box. Likewise, any sparse linear algebra will have lower flops throughput than its dense equivalent, but thats kinda the price you pay for sparse.
What I find very very interesting is that no ones really done a good job of providing sparse linear algebra with any semblance of memory locality. I kinda think that I have a nice story for that, but again, at the end of the day the benchmarks will say.

I at the very least hope the basic tech validates, because there needs to be a good not gpl lin alg suite with good perf for haskell. Hmatrix being gpl has cock blocked the growth of a nice numerics ecosystem on hackage /in haskell for years, and its about time someone puts on some pants and fixes that.

Assuming the tech validates, I really hope the biz validates too (despite me likely making various pieces open source in a BSD3 style way to enrich the community / get hobbyist adoption / other libs written on top, people in haskell land try to avoid using libs that use licenses that arent BSD/MIT/Apache styles), because theres so much more that needs to be done to really have a compelling toolchain for data analysis / numerical computation / machine learning / etc, and I really really like spending my time building better tools. Building the rest of that stack will be outlandishly tractable assuming my linear algebra tech validates having the right regimes of performance on large matrices. (amusingly, no one ever benchmarks linear algebra tools in the 1+gb regime, and i suspect thats because at that point, vectorization means nothing, its all about memory locality memory locality, and a dash of cache aware parallelism).

thats the vague version :)

And thats also not even touching my thoughts on the analytics / data vis tools that go on top. (or the horrifying fact that everyone is eager for better data vis tools, even though most data vis work is about as valuable as designing pretty desktop wall papers to background your power point presentations.... so even if i get everything working... I have a horrifying suspicion that if i allowed unsophisticated folks to use the tools, most of the revenue / interest would be around data vis tooling! Which would mostly be used to provide their customers/end users with pretty pictures that make them feel good but don't help them!)

Point being: i want to be able to say "you understand math, you understand your problem domain, and you can learn stuff. Spend 2-3 weeks playing with haskell and my tools, and you'll be able to focus on applying the math to your problem domain like never before, because you didn't even realize just how terrible most of the current tools out there you were wrestling with are!"


