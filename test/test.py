#!/usr/bin/env python

from selenium import webdriver
from selenium.webdriver.chrome.service import Service as ChromeService
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.firefox.service import Service as FirefoxService

from webdriver_manager.chrome import ChromeDriverManager
from webdriver_manager.firefox import GeckoDriverManager

import subprocess
import pytest
import os


@pytest.fixture
def backend():
    if os.path.exists("eventstore.txt"):
        os.remove("eventstore.txt")
    with subprocess.Popen(["../build/ms"]) as proc:
        proc.is_running = lambda: proc.poll() is None
        yield proc
        proc.terminate()


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


def test(backend):

    chrome = driver("chrome")
    chrome.implicitly_wait(5)

    assert backend.is_running(), "Backend is stopped"
    chrome.get("http://localhost:8080/process-types")
    assert chrome.title == "Process Types"

    assert backend.is_running(), "Backend is stopped"
    input_ = chrome.find_element(By.CLASS_NAME, "input")
    input_.send_keys("New PT" + Keys.ENTER)
    box = chrome.find_element(By.CLASS_NAME, "box")
    assert box.text == "New PT", "The Process Type has not been created"
    assert input_.get_attribute("value") == "", "Form was not cleared after submit"

    chrome.quit()

    assert backend.is_running(), "Backend is stopped"
    firefox = driver("firefox")
    firefox.implicitly_wait(5)
    firefox.get("http://localhost:8080/process-types")
    assert firefox.title == "Process Types"
    firefox.quit()
