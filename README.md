# C3Card

[![Erlang/OTP Version](https://img.shields.io/badge/erlang%2Fotp-%2026-blue)](http://www.erlang.org)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://esl.github.io/c3card/)
[![Latest release](https://img.shields.io/github/v/release/esl/c3card)](https://github.com/esl/c3card/releases)

![c3card](/images/c3card.jpg)

A workshop at [CodeBEAM America 2024](https://codebeamamerica.com/)
showcasing custom hardware and Erlang-based firmware development.

## Table of Contents

  - [Introduction](#introdution)
  - [Hardware](#hardware)
  - [Firmware Overview](#firmware-overview)
  - [Getting Started](#getting-started)
  - [References](#references)
  - [Contributing](#contributing)
  - [License](#license)
  - [Acknowledgements](#acknowledgements)

## Introduction

This project is an exploration into custom hardware design and
manufacturing, and modern firmware development using AtomVM and the
Erlang ecosystem. It is meant to serve as a workshop for the upcoming
[CodeBEAM America 2024](https://codebeamamerica.com/).

Central to the design is the ESP32C3 microcontroller, chosen for it's
RISC-V and WiFi capabilities, as well as it's versatility and full
compatibility with AtomVM.

There is also a Neopixel LED array for it's programmable RGB
capabilities, allowing for a wide range of visual outputs. An AHT20
sensor for cheap, low-powered and high-precision temperature
readings. An SSD1306 OLED display as an effective solution for
real-time data display, offering a 128x64 pixel resolution.

And the last stellar feature of this project is the use of AtomVM, a
new implementation of the BEAM targetted at microcontrollers and
embedded systems. This choice is driven by the desire to leverage all
of the Erlang capabilities and robust features when it comes to
concurrency and fault-tolerant designs in a resource-constrained
environment such as the ESP32.

## Hardware

The overall list of components is the following:

  - SSD1306 OLED display
  - 4 buttons
  - WS2812 Neopixel LED array
  - AHT20 sensor
  - 1 I2C Qwiic connector

And the default IO pinouts are mapped as:

  - IO0: Vbattery sense
  - IO1: Charging flag
  - IO2: I2C SDA
  - IO3: I2C SCL
  - IO4: WS2812 LED
  - IO5: Button 1
  - IO9: Button 2
  - IO7: Button 3
  - IO8: Button 4
  - IO10: OLED Reset

## Firmware Overview

As mentioned before, we are going to implement the application using
Erlang and leveraging the AtomVM BEAM implementation.

The application resembles a regular OTP application structure with
some caveats to take into consideration.

### Dependencies

  - ESP IDF v5.x for compiling AtomVM for the target device
  - rebar3 is used for compiling the application, packing the .beam
    files and flashing the device
  - Erlang/OTP equal or higher than 25

### Installing the AtomVM image

Before installing the application we need to flash AtomVM to the C3
card in order to have a starting image that can run the BEAM files.

You can follow the instructions at the [AtomVM getting started
guide](https://www.atomvm.net/doc/master/getting-started-guide.html).

If you are compiling AtomVM from scratch, you need to compile a couple
of extra components that are not included by default:

  - [Neopixel driver](https://github.com/atomvm/atomvm_neopixel)

## Getting Started

### Building the firmware

To fetch the dependencies and to compile the project, the standard
`rebar3` build tool is used:

```sh
rebar3 compile
```

### Flashing the device

The device port will vary depending on the chipset you use, but
normally will be bound to `/dev/ttyUSB0` or `/dev/ttyACM0`.

```sh
rebar3 atomvm esp32_flash -p $DEVICE_PORT
```

### Documentation

Documentation is provided via `ex_doc`, and can be generated with
rebar3:

```sh
rebar3 ex_doc
```

## References

  - [ESP32C3](https://www.espressif.com/sites/default/files/documentation/esp32-c3_datasheet_en.pdf)
  - [AHT20](https://asairsensors.com/wp-content/uploads/2021/09/Data-Sheet-AHT20-Humidity-and-Temperature-Sensor-ASAIR-V1.0.03.pdf)
  - [SSD1306](https://www.alldatasheet.com/datasheet-pdf/pdf/1179026/ETC2/SSD1306.html)
  - [WS2812 LED](https://www.alldatasheet.com/datasheet-pdf/pdf/553088/ETC2/WS2812.html)
  - [AtomVM](https://atomvm.net)
  - [Ordinatra](https://ordinatra.com/)
  - [ESL](http://erlang-solutions.com/)

## Contributing

We welcome any contributions to the project, via hardware design
enhancements or new software features. Whether you are submitting a
fix or a new feature, your input is valuable. Here is a few guidelines
for making it easier to contribute:

  - Report issues
  - Submit patches
  - Review hardware and software
  - Update documentation
  - Stay engaged

## License

```
Copyright © 2024, Erlang Solutions Ltd.

Apache License, Version 2.0
```

## Acknowledgements

  - [Omer Kilic](https://github.com/omerk) for making this awesome board
  - [Ricardo Lanziano](https://github.com/arpunk) for writing the firmware application
  - [AtomVM](https://atomvm.net) team for their hard work
