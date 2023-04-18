#!/usr/bin/env python

from selenium import webdriver
from selenium.webdriver.chrome.service import Service as ChromeService
from selenium.webdriver.support import expected_conditions as cond
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.firefox.service import Service as FirefoxService
from selenium.webdriver.support.wait import WebDriverWait
from webdriver_manager.chrome import ChromeDriverManager
from webdriver_manager.firefox import GeckoDriverManager
import os
import pytest
import subprocess
import time

ES = "messagestore.txt"
site = "http://localhost:8080"

is_running = lambda proc: proc.poll() is None


@pytest.fixture
def backends():
    plist = []
    if os.path.exists(ES):
        os.remove(ES)
    with subprocess.Popen(["../../build/studio", "-d", "../../build/"]) as proc:
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


def click(browser, text):
    time.sleep(0.1)  # couldn't find a way to wait a detectable change
    # WebDriverWait(browser, timeout=1).until(
    #    cond.presence_of_all_elements_located((By.XPATH, f"//*[text()='{text}']"))
    # )
    return (
        WebDriverWait(browser, timeout=1)
        .until(lambda d: d.find_element(By.XPATH, f"//*[text()='{text}']"))
        .click()
    )


def fill(browser, text):
    return (
        WebDriverWait(browser, timeout=1)
        .until(lambda d: d.find_element(By.TAG_NAME, "input"))
        .send_keys(text + Keys.ENTER)
    )


def find_by_id(browser, text):
    return browser.find_element(By.ID, text)


def find_by_text(browser, text):
    return browser.find_element(By.XPATH, f"//*[contains(text(),'{text}')]")


def text_exists(browser, text):
    assert (
        find_by_text(browser, text).text == text
    ), f"The text '{text}' could not be found"


def add_resource_type(browser, name):
    open_url(browser, "/identifier-type/add")
    assert browser.title == "Adding an Identifier Type"
    click(browser, "Resource Types")
    click(browser, "Next →")
    fill(browser, name)
    click(browser, "Next →")
    click(browser, "Next →")
    click(browser, "Free")
    click(browser, "Validate and finish")


def open_url(browser, path):
    browser.get(site + path)


def test_sync_process_type(backends):
    """Basic test to check event sync between two browsers"""

    assert is_running(backends[-1]), "Backend is stopped"
    chrome = driver("chrome")
    chrome.implicitly_wait(1)

    add_resource_type(chrome, "AAAAAA")
    text_exists(chrome, "AAAAAA")
    WebDriverWait(chrome, 1).until(lambda _: os.path.exists(ES))
    assert count_evstore("AAAAAA") == 1, "(1) Wrong event count in the Event Store"

    # Check we get the identifier type from firefox
    firefox = driver("firefox")
    firefox.implicitly_wait(1)
    open_url(firefox, "/identifier-type/list")
    assert firefox.title == "Identifier Types"

    text_exists(firefox, "AAAAAA")

    # Create another event from chrome and check it appears on firefox w/o reloading
    add_resource_type(chrome, "BBBBBB")
    text_exists(chrome, "BBBBBB")
    text_exists(firefox, "BBBBBB")

    # stop the backend to cut the connection,
    # create and event offline,
    # restart it, check the sync still works for new events
    # and the event created offline is in the backend
    backends[-1].terminate()
    time.sleep(0.5)

    add_resource_type(chrome, "CCCCCC")
    text_exists(chrome, "CCCCCC")
    time.sleep(0.5)

    backends.append(subprocess.Popen(["../../build/studio", "-d", "../../build/"]))

    add_resource_type(chrome, "DDDDDD")
    text_exists(chrome, "DDDDDD")
    assert count_evstore("DDDDDD") == 1, "(2) Wrong event count in the Event Store"
    text_exists(firefox, "DDDDDD")

    WebDriverWait(chrome, 5).until(lambda _: count_evstore("DDDDDD") == 1)

    chrome.set_window_size(1700, 800)
    firefox.set_window_size(1700, 800)

    firefox.quit()
    chrome.quit()
