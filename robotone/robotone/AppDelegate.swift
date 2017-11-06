//
//  AppDelegate.swift
//  robotone
//
//  Created by Khoa Le Tue Nguyen on 9/7/17.
//  Copyright © 2017 Khoa Nguyen. All rights reserved.
//

import Cocoa

@NSApplicationMain
class AppDelegate: NSObject, NSApplicationDelegate {

    @IBOutlet weak var window: NSWindow!
    let pwd = "/Users/KhoaNguyen/Documents/CollegeOfWooster/ISProject/source/is-robotone/"
    

    func applicationDidFinishLaunching(_ aNotification: Notification) {
        // Insert code here to initialize your application
        
    }

    func applicationWillTerminate(_ aNotification: Notification) {
        // Insert code here to tear down your application
    }

    func executeBashCommand(cmd : String, args : String...) -> (output: [String], error: [String], exitCode: Int32){
        
        var output : [String] = []
        var error : [String] = []
        
        let task = Process()
        task.launchPath = cmd
        task.arguments = args
        
        let outpipe = Pipe()
        task.standardOutput = outpipe
        let errpipe = Pipe()
        task.standardError = errpipe
        
        task.launch()
        
        let outdata = outpipe.fileHandleForReading.readDataToEndOfFile()
        if var string = String(data: outdata, encoding: .utf8) {
            string = string.trimmingCharacters(in: .newlines)
            output = string.components(separatedBy: "\n")
        }
        
        let errdata = errpipe.fileHandleForReading.readDataToEndOfFile()
        if var string = String(data: errdata, encoding: .utf8) {
            string = string.trimmingCharacters(in: .newlines)
            error = string.components(separatedBy: "\n")
        }
        
        task.waitUntilExit()
        let status = task.terminationStatus
        
        return (output, error, status)
    }
    
    func runDockerScript() {
        // Call robotone Docker image and execute the proof writing engine
        let (_, _, status) = executeBashCommand(cmd: "/usr/local/bin/docker", args: "exec", "-i", "cont_robotone", "bash", "-c", "cd root-robotone && bash run.sh /root-robotone/build/robotone.tex")
        print(status)
        
        if (status == 0) {
            let (_,_,s0) = executeBashCommand(cmd: "/usr/bin/touch", args: pathToFile.stringValue)
            print(s0)
            
            let (_,_,s1) = executeBashCommand(cmd: "/bin/cp",
                                             args: "-f", pwd + "build/robotone.tex", pathToFile.stringValue)
            print(s1)
            
//            let (out,err,s2) = executeBashCommand(cmd: "/Library/TeX/texbin/xelatex",
//                                             args: "\\input{" + pathToFile.stringValue + "}")
//            
//            print(out, err, s2)
            let (_,_,_) = executeBashCommand(cmd: "/usr/bin/open", args: pathToFile.stringValue)
        }
        
    }
    
    @IBAction func run_docker_image(_ sender: NSButtonCell) {
        sender.isEnabled = false
        runDockerScript()
        sender.isEnabled = true
    }
    
    @IBOutlet var pathToFile: NSTextFieldCell!
    @IBAction func browseFile(_ sender: NSButtonCell) {
        let dialog = NSSavePanel()
        
        dialog.title                   = "Save .tex file";
        dialog.showsResizeIndicator    = true
        dialog.showsHiddenFiles        = false
        dialog.canCreateDirectories    = true
        dialog.allowedFileTypes        = ["tex"]

        
        if (dialog.runModal() == NSModalResponseOK) {
            let result = dialog.url // Pathname of the file
            
            if (result != nil) {
                let path = result!.path
                pathToFile.stringValue = path
            }
        } else {
            // User clicked on "Cancel"
            return
        }
    }
    
    @IBAction func openProblemsFile(_ sender: NSButton) {
        let (_,_,_) = executeBashCommand(cmd: "/usr/bin/open", args: pwd + "src/Problems.hs")
    }
}

