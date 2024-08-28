Feature: Toggle commify in base64 strings

  Background:
    Given I switch to buffer "*commify-test*"
    And I clear the buffer
    And I go to the beginning of the buffer
    And I set variable "commify-hex-prefix-re" to "(0[Xx])"
    And I bind key "C-c ,," to "commify-toggle"

  Scenario: Toggle a single number
    And I insert:
      """
      35e2438eb7feeb28273c4920376fcf296cc83283
      """
    When I press "C-c ,,"
    Then I should see:
      """
      35e2_438e_b7fe_eb28_273c_4920_376f_cf29_6cc8_3283
      """

  Scenario: Double toggle a single number
    When I insert:
      """
      35e2438eb7feeb28273c4920376fcf296cc83283
      938dc9c07a15c330b87ba9627f4b544b93333c9c
      d1ea379b932c640d4e2ac65116f5952e7c7933ed
      """
    And I start an action chain
    And I press "M-<"
    And I press "C-c ,,"
    And I press "C-c ,,"
    And I execute the action chain
    Then I should see:
      """
      35e2438eb7feeb28273c4920376fcf296cc83283
      """

  Scenario: Toggle a whole buffer as a region
    When I insert:
      """
      35e2438eb7feeb28273c4920376fcf296cc83283
      938dc9c07a15c330b87ba9627f4b544b93333c9c
      d1ea379b932c640d4e2ac65116f5952e7c7933ed
      """
    And I call "mark-whole-buffer"
    And I press "C-c ,,"
    And I execute the action chain
    Then I should see:
      """
      35e2_438e_b7fe_eb28_273c_4920_376f_cf29_6cc8_3283
      938d_c9c0_7a15_c330_b87b_a962_7f4b_544b_9333_3c9c
      d1ea_379b_932c_640d_4e2a_c651_16f5_952e_7c79_33ed
      """
