# html-swipe-response

## Overview

This plugin displays HTML content and records responses generated by swipe gestures, keyboard, and button responses. This is ideal for two-alternative forced choice assessments that will be administered on both mobile and desktop platforms. The stimulus can be displayed until a response is given, or for a pre-determined amount of time. The trial can be ended automatically if the subject has failed to respond within a fixed length of time.

Setting the `stimulus_duration` parameter while using the swipe modality can result in a user experience issue, wherein the user must swipe a stimulus div tag that has been hidden after the stimulus duration has elapsed. To solve this, this plugin wraps the stimulus div tag in another tag with the ID `#jspsych-html-swipe-response-stimulus-container`. This div tag can then be styled so that they user has some visual representation of the stimulus even after the `#jspsych-html-swipe-response-stimulus-container` div has been hidden.

## Loading

```js
<script src="https://unpkg.com/@jspsych-contrib/plugin-html-swipe-response@1.1.2"></script>
```

## Compatibility

jsPsych v7.0

## Documentation

See [documentation](docs/jspsych-html-swipe-response.md)

## Author / Citation

[Adam Richie-Halford](https://github.com/richford), [Kruttika Bhat](https://github.com/KruttikaBhat)

Citation:

Pending