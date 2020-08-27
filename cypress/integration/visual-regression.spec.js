describe('Percy Snapshots', () => {
  describe('Colors', () => {
    it('Creates the Colors page snapshot', () => {
      cy.visit('/#Styles/Colors/Colors')
      cy.percySnapshot('Colors')
    })
  })

  describe('Sizes', () => {
    it('Creates a snapshot of large size', () => {
      cy.visit('/#Styles/Sizes/Large')
      cy.percySnapshot('Large size')
    })

    it('Creates a snapshot of medium size', () => {
      cy.visit('/#Styles/Sizes/Medium')
      cy.percySnapshot('Medium size')
    })

    it('Creates a snapshot of small size', () => {
      cy.visit('/#Styles/Sizes/Small')
      cy.percySnapshot('Small size')
    })

    it('Creates a snapshot of medium size', () => {
      cy.visit('/#Styles/Sizes/ExtraSmall')
      cy.percySnapshot('Extra small size')
    })
  })

  describe('Texts', () => {
    it('Creates the Texts page snapshot', () => {
      cy.visit('/#Basics/Texts/Texts')
      cy.percySnapshot('Texts')
    })
  })

  describe('Icons', () => {
    it('Creates the Icons page snapshot', () => {
      cy.visit('/#Basics/Icons/IconsExample')
      cy.percySnapshot('Icons')
    })
  })

  describe('Buttons', () => {
    it('Creates a snapshot for primary variation of a button', () => {
      cy.visit('/#Basics/Buttons/Primary')
      cy.percySnapshot('Primary button')
    })

    it('Creates a snapshot for disabled variation of a button', () => {
      cy.visit('/#Basics/Buttons/Disabled')
      cy.percySnapshot('Disabled button')
    })

    it('Creates a snapshot for success variation of a button', () => {
      cy.visit('/#Basics/Buttons/Success')
      cy.percySnapshot('Success button')
    })

    it('Creates a snapshot for danger variation of a button', () => {
      cy.visit('/#Basics/Buttons/Danger')
      cy.percySnapshot('Danger button')
    })

    it('Creates a snapshot for light variation of a button', () => {
      cy.visit('/#Basics/Buttons/Light')
      cy.percySnapshot('Light button')
    })

    it('Creates a snapshot for clear variation of a button', () => {
      cy.visit('/#Basics/Buttons/Clear')
      cy.percySnapshot('Clear button')
    })

    it('Creates a snapshot for link variation of a button', () => {
      cy.visit('/#Basics/Buttons/Link')
      cy.percySnapshot('Link button')
    })

    it('Creates a snapshot for full-width variation of a button', () => {
      cy.visit('/#Basics/Buttons/Full%20Width')
      cy.percySnapshot('Full-width button')
    })

    it('Creates a snapshot for toggle variation of a button', () => {
      cy.visit('/#Basics/Buttons/Toggle')
      cy.percySnapshot('Toggle button')
    })
  })

  describe('Alerts', () => {
    it('Creates a snapshot for primary variation of an alert', () => {
      cy.visit('/#Basics/Alerts/Primary')
      cy.percySnapshot('Primary alert')
    })

    it('Creates a snapshot for success variation of an alert', () => {
      cy.visit('/#Basics/Alerts/Success')
      cy.percySnapshot('Success alert')
    })

    it('Creates a snapshot for warning variation of an alert', () => {
      cy.visit('/#Basics/Alerts/Warning')
      cy.percySnapshot('Warning alert')
    })

    it('Creates a snapshot for danger variation of an alert', () => {
      cy.visit('/#Basics/Alerts/Danger')
      cy.percySnapshot('Danger alert')
    })
  })

  describe('Badges', () => {
    it('Creates a snapshot for gray variation of a badge', () => {
      cy.visit('/#Basics/Badges/Badge%20gray')
      cy.percySnapshot('Badge gray')
    })

    it('Creates a snapshot for primary variation of a badge', () => {
      cy.visit('/#Basics/Badges/Badge%20primary')
      cy.percySnapshot('Badge primary')
    })

    it('Creates a snapshot for warning variation of a badge', () => {
      cy.visit('/#Basics/Badges/Badge%20warning')
      cy.percySnapshot('Badge warning')
    })

    it('Creates a snapshot for danger variation of a badge', () => {
      cy.visit('/#Basics/Badges/Badge%20danger')
      cy.percySnapshot('Badge danger')
    })

    it('Creates a snapshot for success variation of a badge', () => {
      cy.visit('/#Basics/Badges/Badge%20success')
      cy.percySnapshot('Badge success')
    })
  })

  describe('Text fields', () => {
    it('Creates a snapshot for default text field', () => {
      cy.visit('/#Basics/TextField/Default')
      cy.percySnapshot('Default text field')
    })

    it('Creates a snapshot for username text field', () => {
      cy.visit('/#Basics/TextField/Username')
      cy.percySnapshot('Username text field')
    })

    it('Creates a snapshot for password text field', () => {
      cy.visit('/#Basics/TextField/Password')
      cy.percySnapshot('Password text field')
    })

    it('Creates a snapshot for full-width text field', () => {
      cy.visit('/#Basics/TextField/Full%20Width')
      cy.percySnapshot('Full-width text field')
    })
  })

  describe('Loading spinners', () => {
    it('creates a snapshot for small loading spinners', () => {
      cy.visit('/#Basics/Loading/Small')
      cy.percySnapshot('Small loading spinner')
    })

    it('creates a snapshot for medium loading spinners', () => {
      cy.visit('/#Basics/Loading/Medium')
      cy.percySnapshot('Medium loading spinner')
    })

    it('creates a snapshot for large loading spinners', () => {
      cy.visit('/#Basics/Loading/Large')
      cy.percySnapshot('Large loading spinner')
    })
  })

  describe('Checkboxes', () => {
    it('Creates the Checkboxes page snapshot', () => {
      cy.visit('/#Basics/Checkboxes/Checkboxes')
      cy.percySnapshot('Checkboxes')
    })
  })

  describe('Radio', () => {
    it('Creates the Radio page snapshot', () => {
      cy.visit('/#Basics/Radio/Radio')
      cy.percySnapshot('Radio')
    })
  })

  describe('Tabs', () => {
    it('Creates the Tabs page snapshot', () => {
      cy.visit('/#Basics/Tabs/Tabs')
      cy.percySnapshot('Tabs')
    })
  })

  describe('Tables', () => {
    it('Creates the Desktop Tables page snapshot', () => {
      cy.visit('/#Complex%20components/Tables/Desktop')
      cy.percySnapshot('Tables Desktop')
    })

    it('Creates the Mobile Tables page snapshot', () => {
      cy.visit('/#Complex%20components/Tables/Mobile')
      cy.percySnapshot('Tables Mobile')
    })

    it('Creates the Stateless Tables page snapshot', () => {
      cy.visit('/#Complex%20components/Tables/Stateless')
      cy.percySnapshot('Tables Stateless')
    })

    it('Creates the Selectable Tables page snapshot', () => {
      cy.visit('/#Complex%20components/Tables/Selectable')
      cy.percySnapshot('Tables Selectable')
    })

    it('Creates the Portion Column Tables page snapshot', () => {
      cy.visit('/#Complex%20components/Tables/Portion%20Columns')
      cy.percySnapshot('Tables Portion Column')
    })
  })

  describe('Paginators', () => {
    it('Creates the Paginators page snapshot', () => {
      cy.visit('/#Complex%20components/Paginators/NonNumeric')
      cy.percySnapshot('Paginators')
    })
  })
})
