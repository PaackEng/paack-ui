describe('Percy Snapshots', () => {
  describe('Colors', () => {
    it('Creates the Colors page snapshot', () => {
      cy.visit('/#Styles/Colors/Colors')
      cy.percySnapshot('Colors')
    })
  })

  describe('Sizes', () => {
    it('Creates a snapshot of large size', () => {
      cy.visit('/#Styles/Sizes/United')
      cy.percySnapshot('All sizes')
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
      cy.visit('/#Basics/Buttons/United')
      cy.percySnapshot('All buttons')
    })
  })

  describe('Alerts', () => {
    it('Creates a snapshot for primary variation of an alert', () => {
      cy.visit('/#Basics/Alerts/United')
      cy.percySnapshot('All alerts')
    })
  })

  describe('Badges', () => {
    it('Creates a snapshot for gray variation of a badge', () => {
      cy.visit('/#Basics/Badges/United')
      cy.percySnapshot('All badge')
    })
  })

  describe('Text fields', () => {
    it('Creates a snapshot for default text field', () => {
      cy.visit('/#Basics/TextField/United')
      cy.percySnapshot('All text fields')
    })
  })


  describe('Checkboxes', () => {
    it('Creates the Checkboxes page snapshot', () => {
      cy.visit('/#Basics/Checkboxes/Checkboxes')
      cy.percySnapshot('Checkboxes')
    })
  })

  describe('Radio', () => {
    it('Creates a snapshot for horizontal and vertical arrangement of radio groups', () => {
      cy.visit('/#Basics/Radio/United')
      cy.percySnapshot('Radio groups')
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
