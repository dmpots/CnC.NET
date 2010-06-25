#light

open ExampleGraph



let Step1 = {new IStep1 with
    member self.Compute(tag, b, a, tag2) = 
        let x = tag.Item1
        CnCRuntime.CnCReturn.Ok
}
