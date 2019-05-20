# Assumptions made during SCLM Language Definition migration

* Only **FUNCTN=BUILD** translators are used during the migration. **FUNCTN=PARSE** are used to determine the language that the language definition is building.
* The migration process cannot migrate the SCLM Specific DB2 bind processor **FLMCSPDB**. If a project uses this it will be skipped over with a warning
* Other SCLM Specific compilers prefixed **FLM** will also raise an error message, although the translators will be generated.
* The migration process, once it generates a translator, will compare that translators contents, such as Compiler, Compiler Options, DDs allocated, etc. If it is deemed to be the same, content wise, as a previously generated translator, then a reference will be put into the language definition to the previously generated translator.
* If a translator has the same CALLNAM as a previously generated translator, but is not identical, for example it may have a different compiler option, then the 2nd translator will have a number appended to it's name. This name  with the appended number will also be used in the language definition reference. These names cane be changed post migration if desired. The number used is a unique number related to the order in which the translators are created. 
* If a language definition does not have a FLMINCLS specified then the default source, specified by the user, will be used to generate a default FLMINCLS internally.
* SCLM uses a lot of **@@FLM** variables. Where it makes sense, these have been converted to similarly used values for RTC/DBB. However there are a lot of SCLM **@@FLM** variables that will not be able to map to non-SCLM names. Any renaming of **@@FLM** variables, or cases where a variable cannot be remapped will be reported in the report file.
* For ISPLNK only CMD and PGM are supported in the migration
* For IOTYPE=O or P if there is no **KEYREF** or **DFLTTYP** specified then we cannot derive the output type to be used. As such we will assume that these allocations are the same as **IOTYPE=W** (Temporary).
* For IOTYPE=O or P if there is a KEYREF, but DFLTTYP is blank, but no member specific information can be found for this language and KEYREF combination then we will default the output type to be the KEYREF value. This will be fine for LOAD, OBJ, LIST or LMAP but will be a problem for OUTx. We will check that the type used for IOTYPE=O or P is in the FLMTYPES table and report if it is not. 
* Where SCLM uses **@@FLMDSN(@@FLMMBR)** as input to a translator we cannot really migrate that so we will assume this is translator input.
* During the generation of the link-edit cards, if an Generic ARCHDEF is used in place of a Compilation Control ARCHDEF, to control the compile, then the INCLUDE SYSLIB statement will be derived from the SINC statement. This is because there is no OBJ statement, and it is impossible to work out which OUTx statement relates to the OBJECT deck.
* Link decks will be generated with INCLUDE SYSLIB as it is difficult to understand the alternative DDs used from the SCLM ARCHDEFs. The OBJ type could be used, but there is no guarantee that the OBJ name is the name of the DD to be used for the object deck allocation in place of SYSLIB. 
* If you use your own assembler macros to enhance the standard SCLM macros, for example replacing the data set in a FLMCPYLB with a macro that derives a data set, then this is not supported. In this example the data set definition created will have the name of the macro, which will be incorrect and will need to be manually fixed. 