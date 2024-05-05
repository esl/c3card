# Installer

## Installation instructions

### Requirements

- Compatible Browsers: Chrome or Edge on Desktop. Android support is
  currently not available.

- Required Drivers: Ensure your computer has the necessary drivers
  installed for ESP devices.

### Step-by-Step Guide

#### Initial checklist

- Use Google Chrome or Microsoft Edge on a desktop computer to access
  the installation tools.

- Confirm that your `c3card` is connected to your computer. If the
  device is not recognized, you may need to install additional
  drivers.

#### Install drivers if needed

- If your `c3card` is not detected, it's likely due to missing
  drivers. Below are links to download drivers for the most common USB
  serial chips used in ESP devices:

	- **CP2102 Drivers:** [Download for Windows & Mac](https://www.silabs.com/products/development-tools/software/usb-to-uart-bridge-vcp-drivers)
    - **CH342, CH343, CH9102 Drivers:**
	  - [Download for Windows](https://www.wch.cn/downloads/CH343SER_ZIP.html)
      - [Download for Mac](https://www.wch.cn/downloads/CH34XSER_MAC_ZIP.html)
    - **CH340, CH341 Drivers:**
      - [Download for Windows](https://www.wch.cn/downloads/CH341SER_ZIP.html)
      - [Download for Mac](https://www.wch.cn/downloads/CH341SER_MAC_ZIP.html)

#### Install the c3card firmware

- Follow the on-screen instructions to load the `c3card` firmware onto
  your device.

#### Initial provisioning

- Check the display, which should indicate successful initialization
  and WiFi details for further configuration.

#### Troubleshooting

- **Device Not Recognized:** Ensure that the correct drivers are
  installed and your browser supports the ESP Web Tools.

- **Serial Port Issues:** If the serial port is not visible, this
  typically indicates missing USB serial drivers. Refer to the driver
  installation links provided above.

## Web installer

<div>
  <script type="module"
          src="https://unpkg.com/esp-web-tools@10/dist/web/install-button.js?module">
  </script>

  <esp-web-install-button manifest="manifest.json">
    <button slot="activate">Latest version</button>
  </esp-web-install-button>
</div>
