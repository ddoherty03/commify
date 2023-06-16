Feature: Toggle commify in various numbers

  Background:
    Given I switch to buffer "*commify-test*"
    And I clear the buffer
    And I insert:
      """
      90809823434   -654654654.9879789  654654646.4664E2587
      3.1459  2011-09-22  9/11/2001 It was a bright sunny day
      0x808AC9FE34  -0x808AC9FE34 0b01011011011101110111111
      0000002119  9797987879978 0o5412364123
      <2011-09-22>  [9/11/2001] <2020-07-19 Sun>
      """
    And I go to the beginning of the buffer
    And I bind key "C-c ,," to "commify-toggle"

  Scenario: Toggle a single number
    When I press "C-c ,,"
    Then I should see:
      """
      90,809,823,434   -654654654.9879789  654654646.4664E2587
      3.1459  2011-09-22  9/11/2001 It was a bright sunny day
      0x808AC9FE34  -0x808AC9FE34 0b01011011011101110111111
      0000002119  9797987879978 0o5412364123
      <2011-09-22>  [9/11/2001] <2020-07-19 Sun>
      """

  Scenario: Double toggle a single number
    When I start an action chain
    And I press "M-<"
    And I press "C-c ,,"
    And I press "C-c ,,"
    And I execute the action chain
    Then I should see:
      """
      90809823434   -654654654.9879789  654654646.4664E2587
      3.1459  2011-09-22  9/11/2001 It was a bright sunny day
      0x808AC9FE34  -0x808AC9FE34 0b01011011011101110111111
      0000002119  9797987879978 0o5412364123
      <2011-09-22>  [9/11/2001] <2020-07-19 Sun>
      """

  Scenario: Toggle a whole buffer as a region
    When I start an action chain
    And I press "M-<"
    And I press "C-SPC"
    And I press "M->"
    And I press "C-c ,,"
    And I execute the action chain
    Then I should see:
      """
      90,809,823,434   -654,654,654.9879789  654,654,646.4664E2587
      3.1459  2011-09-22  9/11/2001 It was a bright sunny day
      0x80_8AC9_FE34  -0x80_8AC9_FE34 0b010_1101_1011_1011_1011_1111
      0000002119  9,797,987,879,978 0o54_12_36_41_23
      <2011-09-22>  [9/11/2001] <2020-07-19 Sun>
      """
