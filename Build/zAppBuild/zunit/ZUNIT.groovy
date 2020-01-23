@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.*
import groovy.transform.*


// define script properties
@Field BuildProperties props = BuildProperties.getInstance()
@Field def buildUtils= loadScript(new File("${props.zAppBuildDir}/utilities/BuildUtilities.groovy"))

println("** Building files mapped to ${this.class.getName()}.groovy script")

// verify required test properties
buildUtils.assertBuildProperties(props.zunit_requiredBuildProperties)

// iterate through test list
argMap.testFiles.each { testFile ->
	println "*** zUnit test $testFile"
	
	def member = new CopyToPDS().createMemberName(testFile)
	File logFile = new File("${props.buildOutDir}/${member}.zunit.log")
	if (logFile.exists())
		logFile.delete()

	def zUnitDatasets = props.getFileProperty('zunit_Datasets', testFile)
	def zunit_ConfFolder = props.getFileProperty('zunit_ConfFolder', testFile)
	def resfile = "${props.buildOutDir}/${member}_${props.startTime}.azures"
	def maxRC = props.getFileProperty('zunit_maxRC', testFile).toInteger()

	// the input file should be copied from the source git repository
	//   - not possible from a DBB user build.
	def command = "zunit -c=${zunit_ConfFolder}/${member}.azucfg  -s=$zUnitDatasets -r=$resfile"
	def out = new StringBuffer()
	def err = new StringBuffer()
	
	Process process = command.execute()
	process.waitForProcessOutput( out, err )
	def rc = process.exitValue()
	
	logFile << out.toString().bytes
	
	if(rc > maxRC){
		logFile << err.toString().bytes
		String errorMsg = "*! The test unit return code ($rc) for $testFile exceeded the maximum return code allowed ($maxRC)"
		println(errorMsg)
		props.error = "true"
	}

	def comm = "chtag -tc ISO8859-1 $resfile"
	Process proc = comm.execute()
	
	if ( props.userBuild || props.zUnit )
		println logFile.text
}