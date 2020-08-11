describe('Percy Snapshots', () => {
  describe('Styles', () => {
    it('Creates the Colors page snapshot', () => {
      cy.visit('/#Styles/Colors/Colors')
      cy.percySnapshot('Colors')
    })

    it('Creates the Sizes page snapshot', () => {
      cy.visit('/#Styles/Sizes/Large')
      cy.percySnapshot('Sizes')
    })
  })

  describe('Basics', () => {
    it('Creates the Texts page snapshot', () => {
      cy.visit('/#Basics/Texts/Texts')
      cy.percySnapshot('Texts')
    })

    it('Creates the Icons page snapshot', () => {
      cy.visit('/#Basics/Icons/IconsExample')
      cy.percySnapshot('Icons')
    })

    it('Creates the Buttons page snapshot', () => {
      cy.visit('/#Basics/Buttons/Primary')
      cy.percySnapshot('Buttons')
    })

    it('Creates the Alerts page snapshot', () => {
      cy.visit('/#Basics/Alerts/Primary')
      cy.percySnapshot('Alerts')
    })

    it('Creates the Badges page snapshot', () => {
      cy.visit('/#Basics/Badges/Badge%20gray')
      cy.percySnapshot('Badges')
    })

    it('Creates the TextField page snapshot', () => {
      cy.visit('/#Basics/TextField/Default')
      cy.percySnapshot('TextField')
    })

    it('Creates the Loading page snapshot', () => {
      cy.visit('/#Basics/Loading/Small')
      cy.percySnapshot('Loading')
    })

    it('Creates the Checkboxes page snapshot', () => {
      cy.visit('/#Basics/Checkboxes/Checkboxes')
      cy.percySnapshot('Checkboxes')
    })

    it('Creates the Radio page snapshot', () => {
      cy.visit('/#Basics/Radio/Radio')
      cy.percySnapshot('Radio')
    })
  })

  describe('Complex components', () => {
    it('Creates the Tables page snapshot', () => {
      cy.visit('/#Complex%20components/Tables/Desktop')
      cy.percySnapshot('Tables')
    })

    it('Creates the Paginators page snapshot', () => {
      cy.visit('/#Complex%20components/Paginators/NonNumeric')
      cy.percySnapshot('Paginators')
    })
  })
})
