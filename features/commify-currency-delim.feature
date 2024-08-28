Feature: Toggle commify in various numbers

  Background:
    Given I switch to buffer "*commify-test*"
    And I set commify-hash-enable to nil
    And I set commify-hex-enable to t
    And I set commify-binary-enable to t
    And I clear the buffer
    And I go to the beginning of the buffer
    And I bind key "C-c ,," to "commify-toggle"

  Scenario: Toggle a dollar-value number
    When I insert:
      """
      $90809823434
      $209409283409.00
      """
    When I call "mark-whole-buffer"
    When I press "C-c ,,"
    Then I should see:
      """
      $90,809,823,434
      $209,409,283,409.00
      """

  Scenario: Toggle a dollar-value number with decimal
    When I insert:
      """
      $209409283409.00
      """
    When I call "mark-whole-buffer"
    When I press "C-c ,,"
    Then I should see:
      """
      $209,409,283,409.00
      """

  Scenario: Toggle a euro currency number without a decimal
    When I insert:
      """
      €209409283409
      """
    When I call "mark-whole-buffer"
    When I press "C-c ,,"
    Then I should see:
      """
      €209,409,283,409
      """

  Scenario: Toggle a euro currency number with a decimal
    When I insert:
      """
      €209409283409.67
      """
    When I call "mark-whole-buffer"
    When I press "C-c ,,"
    Then I should see:
      """
      €209,409,283,409.67
      """

  Scenario: Toggle a Yen number with decimal
    When I insert:
      """
      ¥209409283409.00
      """
    When I call "mark-whole-buffer"
    When I press "C-c ,,"
    Then I should see:
      """
      ¥209,409,283,409.00
      """

  Scenario: Toggle a British pound number with decimal
    When I insert:
      """
      £209409283409.00
      """
    When I call "mark-whole-buffer"
    When I press "C-c ,,"
    Then I should see:
      """
      £209,409,283,409.00
      """

  Scenario: Toggle a square-bracket delimited number with decimal
    When I insert:
      """
      [209409283409.00]
      [9/11/2001]
      """
    When I call "mark-whole-buffer"
    When I press "C-c ,,"
    Then I should see:
      """
      [209,409,283,409.00]
      [9/11/2001]
      """

  Scenario: Toggle a brace-delimited number with decimal
    When I insert:
      """
      {209409283409.00}
      """
    When I call "mark-whole-buffer"
    When I press "C-c ,,"
    Then I should see:
      """
      {209,409,283,409.00}
      """

  Scenario: Toggle a paren-delimited number with decimal
    When I insert:
      """
      (209409283409.00)
      """
    When I call "mark-whole-buffer"
    When I press "C-c ,,"
    Then I should see:
      """
      (209,409,283,409.00)
      """

  Scenario: Toggle a < delimited number with decimal
    When I insert:
      """
      <2011-09-22>
      <2020-07-19 Sun>
      <209409283409.00>
      """
    When I call "mark-whole-buffer"
    When I press "C-c ,,"
    Then I should see:
      """
      <2011-09-22>
      <2020-07-19 Sun>
      <209,409,283,409.00>
      """

  Scenario: Toggle a \" delimited number with decimal
    When I insert:
      """
      \"209,409,283,409.00\"
      """
    When I call "mark-whole-buffer"
    When I press "C-c ,,"
    Then I should see:
      """
      \"209409283409.00\"
      """

  Scenario: Toggle a ' delimited number with decimal
    When I insert:
      """
      '209,409,283,409.00'
      """
    When I call "mark-whole-buffer"
    When I press "C-c ,,"
    Then I should see:
      """
      '209409283409.00'
      """

  Scenario: Toggle a region with currency and delimited numbers
    When I insert:
      """
      $90809823434
      $209409283409.00
      ₠209409283409.00
      €209409283409.00
      ¥209409283409.00
      £209409283409.00
      [209409283409.00]
      {209409283409.00}
      (209409283409.00)
      <209409283409.00>
      \"209409283409.00\"
      '209409283409.00'
      A209409283409.00
      209409283409.00
      <2011-09-22>
      [9/11/2001]
      <2020-07-19 Sun>
      """
    When I call "mark-whole-buffer"
    When I press "C-c ,,"
    Then I should see:
      """
      $90,809,823,434
      $209,409,283,409.00
      ₠209,409,283,409.00
      €209,409,283,409.00
      ¥209,409,283,409.00
      £209,409,283,409.00
      [209,409,283,409.00]
      {209,409,283,409.00}
      (209,409,283,409.00)
      <209,409,283,409.00>
      \"209,409,283,409.00\"
      '209,409,283,409.00'
      A209409283409.00
      209,409,283,409.00
      <2011-09-22>
      [9/11/2001]
      <2020-07-19 Sun>
      """

  Scenario: Re-Toggle a region with currency and delimited numbers
    When I insert:
      """
      $90,809,823,434
      $209,409,283,409.00
      ₠209,409,283,409.00
      €209,409,283,409.00
      ¥209,409,283,409.00
      £209,409,283,409.00
      [209,409,283,409.00]
      {209,409,283,409.00}
      (209,409,283,409.00)
      <209,409,283,409.00>
      \"209,409,283,409.00\"
      '209,409,283,409.00'
      A209409283409.00
      209409283409.00
      <2011-09-22>
      [9/11/2001]
      <2020-07-19 Sun>
      """
    When I call "mark-whole-buffer"
    When I press "C-c ,,"
    Then I should see:
      """
      $90809823434
      $209409283409.00
      ₠209409283409.00
      €209409283409.00
      ¥209409283409.00
      £209409283409.00
      [209409283409.00]
      {209409283409.00}
      (209409283409.00)
      <209409283409.00>
      \"209409283409.00\"
      '209409283409.00'
      A209409283409.00
      209,409,283,409.00
      <2011-09-22>
      [9/11/2001]
      <2020-07-19 Sun>
      """
