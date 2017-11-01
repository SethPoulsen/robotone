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


    func applicationDidFinishLaunching(_ aNotification: Notification) {
        // Insert code here to initialize your application
    }

    func applicationWillTerminate(_ aNotification: Notification) {
        // Insert code here to tear down your application
    }

    func bashCommandCall(_ path: String, _ arguments: [String]) {
        let task = Process.launchedProcess(launchPath: path, arguments: arguments)
        task.waitUntilExit()
    }
    
    @IBAction func load_proofs(_ sender: NSButtonCell) {
        
    }
    
    @IBAction func run_docker_image(_ sender: NSButtonCell) {
        let path = "/usr/local/bin/docker"
        // docker exec -i cont_robotone bash -c "cd root-robotone && bash run.sh /root-robotone/build/$outfile"
        let arguments = ["exec", "-i", "cont_robotone", "bash", "-c", "cd root-robotone && bash run.sh /root-robotone/build/$outfile"]
        
        sender.isEnabled = false
        bashCommandCall(path, arguments)
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
}

