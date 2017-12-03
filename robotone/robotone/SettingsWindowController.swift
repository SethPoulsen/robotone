//
//  SettingsWindowController.swift
//  robotone
//
//  Created by Khoa Le Tue Nguyen on 11/6/17.
//  Copyright © 2017 Khoa Nguyen. All rights reserved.
//

import Cocoa

class SettingsWindowController: NSWindowController {

    override var windowNibName: String? { return "SettingsWindowController" }
    override var owner: AnyObject { return self }
    @IBOutlet var pathToRobotoneFolder: NSTextField!
    
    // vars for manipulating .plist file
    var resourcePaths: NSMutableDictionary?
    var plistPathInDocument:String = String()
    // --
    
    override func windowDidLoad() {
        //super.windowDidLoad()

        // Implement this method to handle any initialization after your window controller's window has been loaded from its nib file.
        
        // placeholder text
        let documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] as String
        let plistPathInDocument = documentsPath + "/RobotonePaths.plist"
        if !FileManager.default.fileExists(atPath: plistPathInDocument){
            pathToRobotoneFolder.stringValue = documentsPath
        }
        else {
            resourcePaths = NSMutableDictionary(contentsOfFile: plistPathInDocument)
            pathToRobotoneFolder.stringValue = resourcePaths?.object(forKey: "pwd") as! String
        }

    }
    
    @IBAction func browseDirectory(_ sender: NSButtonCell) {
        let dialog = NSOpenPanel()
        
        dialog.title                   = "Select Folder";
        dialog.showsResizeIndicator    = true
        dialog.showsHiddenFiles        = false
        dialog.canCreateDirectories    = true
        dialog.canChooseDirectories    = true
        
        if (dialog.runModal() == NSModalResponseOK) {
            let result = dialog.url // Pathname of the file
            
            if (result != nil) {
                let path = result!.path
                pathToRobotoneFolder.stringValue = path
                
                let documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] as String
                let plistPathInDocument = documentsPath + "/RobotonePaths.plist"
                // Update .plist file
                if !FileManager.default.fileExists(atPath: plistPathInDocument){
                    let plistPathInBundle = Bundle.main.path(forResource: "RobotonePaths", ofType: "plist") as String!
                    do {
                        try FileManager.default.copyItem(atPath: plistPathInBundle!, toPath: plistPathInDocument)
                    } catch {
                        print("Error occurred while copying file to document \(error)")
                    }
                }
                resourcePaths = NSMutableDictionary(contentsOfFile: plistPathInDocument)
                resourcePaths?["pwd"] = path
                print(resourcePaths?["pwd"] as! String)
                resourcePaths?.write(toFile: plistPathInDocument, atomically: true)
                
                // debug
//                print(plistPathInDocument)
//                let tmp: NSDictionary = NSDictionary(contentsOfFile: plistPathInDocument)!
//                print("pwd = ", tmp.object(forKey: "pwd") as! String)

            }
        } else {
            // User clicked on "Cancel"
            return
        }
    }

    @IBAction func startDockerContainer(_ sender: NSButton) {
        let docker = resourcePaths?.object(forKey: "docker") as! String
        let pwd = resourcePaths?.object(forKey: "pwd") as! String
        
        // #docker build -t robotone . # building new Docker image
        let (_,e1,s1) = Helper.executeBashCommand(cmd: docker, args: "build", "--file", pwd + "/Dockerfile", "-t", "robotone", pwd)
        
        // # docker create -v $(pwd):/root-robotone --name cont_robotone robotone /bin/bash
        // docker run -v $(pwd):/root-robotone --name cont_robotone -it robotone /bin/bash &
        let (_,e2,s2) = Helper.executeBashCommand(cmd: docker, args: "create", "-v", pwd + ":/root-robotone", "--name", "cont_robotone", "robotone", "/bin/bash")
        
        // # docker container start cont_robotone
        let (_,e3,s3) = Helper.executeBashCommand(cmd: docker, args: "container", "start", "cont_robotone")
//        let task = Process()
//        task.launchPath = docker
//        task.arguments = ["container", "start", "cont_robotone"]
//        task.launch()
//
        print(s1, s2, s3)
        print(e1)
        print(e2)
        print(e3)
    }
}
