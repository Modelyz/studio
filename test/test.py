#!/usr/bin/env python

from selenium import webdriver
from selenium.webdriver.chrome.service import Service as ChromeService
from selenium.webdriver.firefox.service import Service as FirefoxService

from webdriver_manager.chrome import ChromeDriverManager
from webdriver_manager.firefox import GeckoDriverManager

import subprocess
import pytest
import time


@pytest.fixture
def backend():
    backend = subprocess.Popen(["../build/ms"])
    time.sleep(0.2)
    yield backend
    backend.terminate()
    backend.wait()


def test_chrome(backend):
    assert backend.poll() is None, "Backend is not running"
    service = ChromeService(executable_path=ChromeDriverManager().install())

    driver = webdriver.Chrome(service=service)

    driver.quit()


def test_firefox(backend):
    assert backend.poll() is None, "Backend is not running"
    service = FirefoxService(executable_path=GeckoDriverManager().install())

    driver = webdriver.Firefox(service=service)

    driver.quit()
