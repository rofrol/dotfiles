ipconfig getifaddr $(route get default | awk '/interface: / {print $2}')
