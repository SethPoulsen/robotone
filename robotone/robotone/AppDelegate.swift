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
    var settingsWindowController: SettingsWindowController?
    var resourcePaths: NSDictionary?
    var pwd: String = ""
    var plistPathInDocument: String = ""
    
    func applicationDidFinishLaunching(_ aNotification: Notification) {
        // Insert code here to initialize your application
        
        // Detect first time setup by checking for existence of .plist file
        let documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] as String
        plistPathInDocument = documentsPath + "/RobotonePaths.plist"
        if !FileManager.default.fileExists(atPath: plistPathInDocument){
            let _ = Helper.dialogOKCancel(question: "First-time Setup Required", text: "Please click Settings to set up appropriate configurations for robotone.")
        }
        else {
            resourcePaths = NSDictionary(contentsOfFile: plistPathInDocument)
        }
        
        // placeholder text for path text field
        pathToFile.stringValue = documentsPath + "/proofs.tex"
    }

    func applicationWillTerminate(_ aNotification: Notification) {
        // Insert code here to tear down your application
    }

    func runDockerScript() {
        // Pull paths of docker & xelatex binaries from RobotonePaths.plist
        resourcePaths = NSDictionary(contentsOfFile: plistPathInDocument)
        let docker = resourcePaths?.object(forKey: "docker") as! String
        let xelatex = resourcePaths?.object(forKey: "xelatex") as! String
        
        // Call robotone Docker image and execute the proof writing engine
        let (_, _, status) = Helper.executeBashCommand(cmd: docker, args: "exec", "-i", "cont_robotone", "bash", "-c", "cd root-robotone && bash run.sh /root-robotone/build/robotone.tex")
//        print(status)
        
        if (status == 0) {
            
            //    let pwd = "/Users/KhoaNguyen/Documents/CollegeOfWooster/ISProject/source/is-robotone/"
            pwd = resourcePaths?.object(forKey: "pwd") as! String
            
            let dir = (pathToFile.stringValue as NSString).deletingLastPathComponent + "/"
            let fileName = (pathToFile.stringValue as NSString).lastPathComponent
            
            let (_,_,_) = Helper.executeBashCommand(cmd: "/usr/bin/touch", args: pathToFile.stringValue)
            let (_,_,_) = Helper.executeBashCommand(cmd: "/bin/cp", args: "-f",
                                                    pwd + "/build/robotone.tex", pathToFile.stringValue)

            let (_,_,_) = Helper.executeBashCommand(cmd: "/bin/cp",
                                                    args: "-f", pwd + "/build/robotone.tex", fileName)
            
            let (_,_,_) = Helper.executeBashCommand(cmd: xelatex,
                                                    args: "\\input{" + fileName + "}")
//            print(s2)
            
            let pdfFileName = (fileName as NSString).deletingPathExtension + ".pdf"
            let (_,_,_) = Helper.executeBashCommand(cmd: "/usr/bin/touch", args: dir + pdfFileName)
            let (_,_,_) = Helper.executeBashCommand(cmd: "/bin/cp",
                                                    args: "-f", pdfFileName, dir + pdfFileName)
//            print(s3)
            
            let (_,_,_) = Helper.executeBashCommand(cmd: "/usr/bin/open", args: dir + pdfFileName)
            
            // clean temp files
            let fileManager = FileManager.default
            
            do {
                let bareFileName = (fileName as NSString).deletingPathExtension
                try fileManager.removeItem(atPath: bareFileName + ".pdf")
                try fileManager.removeItem(atPath: bareFileName + ".tex")
                try fileManager.removeItem(atPath: bareFileName + ".log")
                try fileManager.removeItem(atPath: bareFileName + ".aux")
            }
            catch let error as NSError {
                print("Ooops! Something went wrong: \(error)")
            }
            
        }
        else {
            let _ = Helper.dialogOKCancel(question: "Error", text: "The robotone Docker container is not running properly. Please review Problems.hs or your current Settings.")
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
        resourcePaths = NSDictionary(contentsOfFile: plistPathInDocument)
        pwd = resourcePaths?.object(forKey: "pwd") as! String

        let (_,_,_) = Helper.executeBashCommand(cmd: "/usr/bin/open", args: pwd + "/src/Problems.hs")
    }
    
    @IBAction func openSettings(_ sender: NSButtonCell) {
        settingsWindowController = SettingsWindowController(windowNibName: "SettingsWindowController")
        settingsWindowController!.loadWindow()
        settingsWindowController!.windowDidLoad()
        settingsWindowController!.showWindow(self)
    }
}

