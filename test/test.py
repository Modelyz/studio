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


def count_evstore(string):
    with open(ES) as es:
        return es.read().count(string)


def test_sync_process_type(backends):
    """Basic test to check event sync between two browsers"""

    assert backends[-1].is_running(), "Backend is stopped"
    chrome = driver("chrome")
    chrome.implicitly_wait(5)

    chrome.get("http://localhost:8080/process-types")
    assert chrome.title == "Process Types"

    # Create an event and check it is stored in the backend
    chrome.find_element(By.CLASS_NAME, "input").send_keys("AAAAAA" + Keys.ENTER)
    assert (
        chrome.find_element(By.ID, "AAAAAA").text == "AAAAAA"
    ), "The Process Type has not been created"
    assert (
        chrome.find_element(By.CLASS_NAME, "input").get_attribute("value") == ""
    ), "Form was not cleared after submit"
    WebDriverWait(chrome, 2).until(lambda _: os.path.exists(ES))
    assert count_evstore("AAAAAA") == 1, "(1) Wrong event count in the Event Store"

    # Check we get the process type from another browser
    firefox = driver("firefox")
    firefox.implicitly_wait(5)
    firefox.get("http://localhost:8080/process-types")
    assert firefox.title == "Process Types"

    assert (
        firefox.find_element(By.ID, "AAAAAA").text == "AAAAAA"
    ), "The Process Type has not been created"

    # Create another event from chrome and check it appears on firefox w/o reloading
    chrome.find_element(By.CLASS_NAME, "input").send_keys("Bééé" + Keys.ENTER)
    assert (
        chrome.find_element(By.ID, "Bééé").text == "Bééé"
    ), "The Process Type has not been created on Chrome"
    assert (
        firefox.find_element(By.ID, "Bééé").text == "Bééé"
    ), "The Process Type has not been created on Firefox"

    # stop the backend to cut the connection,
    # create and event offline,
    # restart it, check the sync still works for new events
    # and the event created offline is in the backend
    backends[-1].terminate()
    time.sleep(0.5)
    chrome.find_element(By.CLASS_NAME, "input").send_keys(
        "EventCreatedWithoutBackend" + Keys.ENTER
    )
    time.sleep(0.5)
    backends.append(subprocess.Popen(["../build/ms"]))
    chromestatus = chrome.find_element(By.ID, "WSStatus")
    WebDriverWait(chrome, 10).until(lambda _: chromestatus.text == "WS=WSOpen")

    input_ = chrome.find_element(By.CLASS_NAME, "input")
    input_.send_keys("Cééé" + Keys.ENTER)
    assert (
        chrome.find_element(By.ID, "Cééé").text == "Cééé"
    ), "The Process Type has not been created on Chrome"
    assert count_evstore("Cééé") == 1, "(2) Wrong event count in the Event Store"
    assert (
        firefox.find_element(By.ID, "Cééé").text == "Cééé"
    ), "The Process Type has not been created on Firefox"

    WebDriverWait(chrome, 10).until(
        lambda _: count_evstore("EventCreatedWithoutBackend") == 1
    )

    firefox.quit()
    chrome.quit()
