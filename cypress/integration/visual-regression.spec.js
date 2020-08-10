describe("Integration test with visual testing", () => {
  it("Colors Page Snapshot", () => {
    cy.visit("/#Styles/Colors/Colors");
    cy.percySnapshot("colors");
  });

  it("Sizes Page Snapshot", () => {
    cy.visit("/#Styles/Sizes/Large");
    cy.percySnapshot("sizes");
  });
});
