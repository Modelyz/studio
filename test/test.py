#!/usr/bin/env python

from selenium import webdriver
from selenium.webdriver.chrome.service import Service as ChromeService
import time
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.firefox.service import Service as FirefoxService
from selenium.webdriver.support.ui import WebDriverWait
from webdriver_manager.chrome import ChromeDriverManager
from webdriver_manager.firefox import GeckoDriverManager
import os
import pytest
import subprocess

ES = "eventstore.txt"


@pytest.fixture
def backends():
    plist = []
    if os.path.exists(ES):
        os.remove(ES)
    with subprocess.Popen(["../build/ms"]) as proc:
        proc.is_running = lambda: proc.poll() is None
        plist.append(proc)
        yield plist
        for p in plist:
            p.terminate()
        for p in plist:
            p.wait()


def driver(nav):
    if nav == "chrome":
        service = ChromeService(executable_path=ChromeDriverManager().install())
        driver = webdriver.Chrome(service=service)
    elif nav == "firefox":
        service = FirefoxService(executable_path=GeckoDriverManager().install())
        driver = webdriver.Firefox(service=service)
    else:
        raise NotImplementedError
    return driver


def test_sync_process_type(backends):
    """Basic test to check event sync between two browsers"""

    assert backends[-1].is_running(), "Backend is stopped"
    chrome = driver("chrome")
    chrome.implicitly_wait(5)

    chrome.get("http://localhost:8080/process-types")
    assert chrome.title == "Process Types"

    # Create an event and check it is stored in the backend
    input_ = chrome.find_element(By.CLASS_NAME, "input")
    input_.send_keys("AAAAAA" + Keys.ENTER)
    assert (
        chrome.find_element(By.ID, "AAAAAA").text == "AAAAAA"
    ), "The Process Type has not been created"
    input_ = chrome.find_element(By.CLASS_NAME, "input")
    assert input_.get_attribute("value") == "", "Form was not cleared after submit"
    WebDriverWait(chrome, 2).until(lambda _: os.path.exists(ES))
    with open(ES) as es:
        assert es.read().find("AAAAAA") > 0, "Event not stored in the backend"

    # Check we get the process type from another browser
    firefox = driver("firefox")
    firefox.implicitly_wait(5)
    firefox.get("http://localhost:8080/process-types")
    assert firefox.title == "Process Types"

    assert (
        firefox.find_element(By.ID, "AAAAAA").text == "AAAAAA"
    ), "The Process Type has not been created"

    # Create another event from chrome and check it appears on firefox w/o reloading
    input_ = chrome.find_element(By.CLASS_NAME, "input")
    input_.send_keys("Bééé" + Keys.ENTER)
    assert (
        chrome.find_element(By.ID, "Bééé").text == "Bééé"
    ), "The Process Type has not been created on Chrome"
    assert (
        firefox.find_element(By.ID, "Bééé").text == "Bééé"
    ), "The Process Type has not been created on Firefox"

    # stop the backend to cut the connection,
    # restart it, check the sync still works
    backends[-1].terminate()
    time.sleep(0.5)
    backends.append(subprocess.Popen(["../build/ms"]))
    chromestatus = chrome.find_element(By.ID, "WSStatus")
    WebDriverWait(chrome, 10).until(lambda _: chromestatus.text == "WSOnline")

    input_ = chrome.find_element(By.CLASS_NAME, "input")
    input_.send_keys("Cééé" + Keys.ENTER)
    assert (
        chrome.find_element(By.ID, "Cééé").text == "Cééé"
    ), "The Process Type has not been created on Chrome"
    with open(ES) as es:
        assert es.read().find("Cééé") > 0, "Event not stored in the backend"
    assert (
        firefox.find_element(By.ID, "Cééé").text == "Cééé"
    ), "The Process Type has not been created on Firefox"
    backends[-1].terminate()

    firefox.quit()
    chrome.quit()
