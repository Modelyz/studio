from common import (
    backends,
    add_identifier_type,
    driver,
    text_exists,
    count_evstore,
    open_url,
)
from selenium.webdriver.support.wait import WebDriverWait
import os
import pytest
import subprocess
import time

ES = "messagestore.txt"
site = "http://localhost:8080"

is_running = lambda proc: proc.poll() is None


backends = pytest.fixture(backends)


def test_sync_process_type(backends):
    """Basic test to check event sync between two browsers"""

    assert is_running(backends[-1]), "Backend is stopped"
    chrome = driver("chrome")
    chrome.implicitly_wait(1)

    add_identifier_type(chrome, ["Resource Types"], "AAAAAA")
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
    add_identifier_type(chrome, ["Resource Types"], "BBBBBB")
    text_exists(chrome, "BBBBBB")
    text_exists(firefox, "BBBBBB")

    # stop the backend to cut the connection,
    # create and event offline,
    # restart it, check the sync still works for new events
    # and the event created offline is in the backend
    backends[-1].terminate()
    time.sleep(0.5)

    add_identifier_type(chrome, ["Resource Types"], "CCCCCC")
    text_exists(chrome, "CCCCCC")
    time.sleep(0.5)

    backends.append(subprocess.Popen(["../../build/studio", "-d", "../../build/"]))

    add_identifier_type(chrome, ["Resource Types"], "DDDDDD")
    text_exists(chrome, "DDDDDD")
    assert count_evstore("DDDDDD") == 1, "(2) Wrong event count in the Event Store"
    text_exists(firefox, "DDDDDD")

    WebDriverWait(chrome, 5).until(lambda _: count_evstore("DDDDDD") == 1)

    chrome.set_window_size(1700, 800)
    firefox.set_window_size(1700, 800)

    firefox.quit()
    chrome.quit()
