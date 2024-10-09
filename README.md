# runoff
System to do text processing using .COMMAND form instructions.

Technically, this isn't the "actual official" Runoff, since it was not developed by Digital Equipment Corporation (DEC), but then again, "technically" DEC doesn't exist anymore, having been bought by COMPAQ then COMPAQ being bought by Hewlett Packard, and nobody uses the old software except hobbyists, it's probably safe to "acquire" the name due to abandonment.

Now, it might be worth asking why someone would want to use something like this when there are plenty of word processors out that do a great job. Well, sometimes you can't get exactly what you want and might need finer control. Or you found older documents that use this format. Or you just like the idea.

The original program was part of the Stanford Pascal compiler, run-time system, and examples for the IBM 370 mainframe under MVS and OS/VS 1 and OS/VS 2. This was part of the sample programs that were included. This came from the TK5 MVS distribution that runs on the Hercules mainframe emulator. For those interested, they came from a PDS (IBM's equivalent of the Unix/Linux tar file format) dataset named PASCAL.PASSAMP and were in the members RNF (for the program) and RNFDOC (for the documentation.) They are located on disc pack TK5002, the physical file name is dasd/tk5002.299 in the TK5 distribution.

Files included
RNF.PAS - Original RNF from the samples collection
RNFDOC.DOC - Output document describing the features
RNFDOCIN.RNF - Original source with Runoff markup
RUNOFF.LPI - RNF being converted to a version that will compile with and run under Free Pascal.
RUNOFF.LPR
RUNOFF.LPS

As an extra bonus, I've included the files from the PASSAMP collection. They are in the directory of that name.


Paul Robinson
October 9, 2024