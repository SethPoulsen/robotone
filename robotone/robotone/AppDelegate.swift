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

    @IBAction func load_proofs(_ sender: NSButtonCell) {
        
    }
    @IBAction func build_docker_image(_ sender: NSButtonCell) {
        let path = "/usr/local/bin/docker"
        let arguments = ["build", "-t", "robotone", "."]
        
        sender.isEnabled = false
        let task = Process.launchedProcess(launchPath: path, arguments: arguments)
        task.waitUntilExit()
        sender.isEnabled = true
    }
}

