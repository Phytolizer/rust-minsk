use crate::diagnostic::DiagnosticBag;
use crate::plumbing::Object;
use crate::plumbing::ObjectKind;
use crate::syntax::expressions::AssignmentExpressionSyntax;
use crate::syntax::expressions::BinaryExpressionSyntax;
use crate::syntax::expressions::ExpressionSyntaxRef;
use crate::syntax::expressions::LiteralExpressionSyntax;
use crate::syntax::expressions::NameExpressionSyntax;
use crate::syntax::expressions::ParenthesizedExpressionSyntax;
use crate::syntax::expressions::UnaryExpressionSyntax;
use crate::syntax::statements::BlockStatementSyntax;
use crate::syntax::statements::ExpressionStatementSyntax;
use crate::syntax::statements::IfStatementSyntax;
use crate::syntax::statements::StatementSyntaxRef;
use crate::syntax::statements::VariableDeclarationStatementSyntax;
use crate::syntax::statements::WhileStatementSyntax;
use crate::syntax::CompilationUnitSyntaxRef;
use crate::syntax::SyntaxKind;
use crate::syntax::SyntaxNodeRef;
use crate::text::VariableSymbol;

use self::operators::BoundBinaryOperator;
use self::operators::BoundUnaryOperator;
use self::scope::BoundGlobalScope;
use self::scope::BoundScope;

mod operators;
pub(crate) mod scope;

pub(crate) enum BoundNodeKind {
    BinaryExpression,
    UnaryExpression,
    LiteralExpression,
    VariableExpression,
    AssignmentExpression,

    BlockStatement,
    ExpressionStatement,
    IfStatement,
    VariableDeclarationStatement,
    WhileStatement,
}

pub(crate) enum BoundNode {
    Expression(BoundExpression),
    Statement(BoundStatement),
}

impl BoundNode {
    pub(crate) fn kind(&self) -> BoundNodeKind {
        match self {
            BoundNode::Expression(e) => e.kind(),
            BoundNode::Statement(s) => s.kind(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum BoundStatement {
    Block(BoundBlockStatement),
    Expression(BoundExpressionStatement),
    If(BoundIfStatement),
    VariableDeclaration(BoundVariableDeclarationStatement),
    While(BoundWhileStatement),
}

impl BoundStatement {
    fn kind(&self) -> BoundNodeKind {
        match self {
            BoundStatement::Block(_) => BoundNodeKind::BlockStatement,
            BoundStatement::Expression(_) => BoundNodeKind::ExpressionStatement,
            BoundStatement::If(_) => BoundNodeKind::IfStatement,
            BoundStatement::VariableDeclaration(_) => BoundNodeKind::VariableDeclarationStatement,
            BoundStatement::While(_) => BoundNodeKind::WhileStatement,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct BoundBlockStatement {
    pub(crate) statements: Vec<BoundStatement>,
}

#[derive(Debug, Clone)]
pub(crate) struct BoundExpressionStatement {
    pub(crate) expression: BoundExpression,
}

#[derive(Debug, Clone)]
pub(crate) struct BoundIfStatement {
    pub(crate) condition: BoundExpression,
    pub(crate) then_statement: Box<BoundStatement>,
    pub(crate) else_statement: Option<Box<BoundStatement>>,
}

#[derive(Debug, Clone)]
pub(crate) struct BoundVariableDeclarationStatement {
    pub(crate) variable: VariableSymbol,
    pub(crate) initializer: BoundExpression,
}

#[derive(Debug, Clone)]
pub(crate) struct BoundWhileStatement {
    pub(crate) condition: BoundExpression,
    pub(crate) body: Box<BoundStatement>,
}

#[derive(Debug, Clone)]
pub(crate) enum BoundExpression {
    Binary(BoundBinaryExpression),
    Unary(BoundUnaryExpression),
    Literal(BoundLiteralExpression),
    Variable(BoundVariableExpression),
    Assignment(BoundAssignmentExpression),
}

impl BoundExpression {
    pub(crate) fn kind(&self) -> BoundNodeKind {
        match self {
            BoundExpression::Binary(_) => BoundNodeKind::BinaryExpression,
            BoundExpression::Unary(_) => BoundNodeKind::UnaryExpression,
            BoundExpression::Literal(_) => BoundNodeKind::LiteralExpression,
            BoundExpression::Variable(_) => BoundNodeKind::VariableExpression,
            BoundExpression::Assignment(_) => BoundNodeKind::AssignmentExpression,
        }
    }

    pub(crate) fn get_type(&self) -> ObjectKind {
        match self {
            BoundExpression::Binary(e) => e.operator.result_type,
            BoundExpression::Unary(e) => e.operator.result_type,
            BoundExpression::Literal(e) => e.value.kind(),
            BoundExpression::Variable(e) => e.variable.kind,
            BoundExpression::Assignment(e) => e.expression.get_type(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum BoundBinaryOperatorKind {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Equality,
    Inequality,
    Less,
    LessOrEquals,
    Greater,
    GreaterOrEquals,
}

#[derive(Debug, Clone)]
pub struct BoundBinaryExpression {
    pub(crate) left: Box<BoundExpression>,
    pub(crate) operator: &'static BoundBinaryOperator,
    pub(crate) right: Box<BoundExpression>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum BoundUnaryOperatorKind {
    Identity,
    Negation,
    LogicalNegation,
    BitwiseNegation,
}

#[derive(Debug, Clone)]
pub(crate) struct BoundUnaryExpression {
    pub(crate) operator: &'static BoundUnaryOperator,
    pub(crate) operand: Box<BoundExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct BoundLiteralExpression {
    pub(crate) value: Object,
}

#[derive(Debug, Clone)]
pub(crate) struct BoundVariableExpression {
    pub(crate) variable: VariableSymbol,
}

#[derive(Debug, Clone)]
pub(crate) struct BoundAssignmentExpression {
    pub(crate) variable: VariableSymbol,
    pub(crate) expression: Box<BoundExpression>,
}

pub(crate) struct Binder {
    pub(crate) diagnostics: DiagnosticBag,
    scope: BoundScope,
}

impl Binder {
    pub(crate) fn bind_statement(&mut self, statement: StatementSyntaxRef) -> Box<BoundStatement> {
        match statement {
            StatementSyntaxRef::Block(s) => self.bind_block_statement(s),
            StatementSyntaxRef::Expression(s) => self.bind_expression_statement(s),
            StatementSyntaxRef::If(s) => self.bind_if_statement(s),
            StatementSyntaxRef::VariableDeclaration(s) => {
                self.bind_variable_declaration_statement(s)
            }
            StatementSyntaxRef::While(s) => self.bind_while_statement(s),
        }
    }

    pub(crate) fn bind_expression(
        &mut self,
        expression: ExpressionSyntaxRef,
    ) -> Box<BoundExpression> {
        match expression {
            ExpressionSyntaxRef::Binary(e) => self.bind_binary_expression(e),
            ExpressionSyntaxRef::Unary(e) => self.bind_unary_expression(e),
            ExpressionSyntaxRef::Literal(e) => self.bind_literal_expression(e),
            ExpressionSyntaxRef::Parenthesized(e) => self.bind_parenthesized_expression(e),
            ExpressionSyntaxRef::Name(e) => self.bind_name_expression(e),
            ExpressionSyntaxRef::Assignment(e) => self.bind_assignment_expression(e),
        }
    }

    fn bind_expression_with_type(
        &mut self,
        expression: ExpressionSyntaxRef,
        target_type: ObjectKind,
    ) -> Box<BoundExpression> {
        let result = self.bind_expression(expression);
        if result.get_type() != target_type {
            self.diagnostics.report_cannot_convert(
                SyntaxNodeRef::Expression(expression).span(),
                result.get_type(),
                target_type,
            );
        }
        result
    }

    fn bind_binary_expression(&mut self, e: &BinaryExpressionSyntax) -> Box<BoundExpression> {
        let left = self.bind_expression(e.left.create_ref());
        let right = self.bind_expression(e.right.create_ref());
        let operator =
            BoundBinaryOperator::bind(e.operator_token.kind, left.get_type(), right.get_type());
        if let Some(operator) = operator {
            Box::new(BoundExpression::Binary(BoundBinaryExpression {
                left,
                operator,
                right,
            }))
        } else {
            self.diagnostics.report_undefined_binary_operator(
                e.operator_token.span(),
                e.operator_token.text.clone(),
                left.get_type(),
                right.get_type(),
            );
            left
        }
    }

    fn bind_unary_expression(&mut self, e: &UnaryExpressionSyntax) -> Box<BoundExpression> {
        let operand = self.bind_expression(e.operand.create_ref());
        let operator = BoundUnaryOperator::bind(e.operator_token.kind, operand.get_type());
        if let Some(operator) = operator {
            Box::new(BoundExpression::Unary(BoundUnaryExpression {
                operator,
                operand,
            }))
        } else {
            self.diagnostics.report_undefined_unary_operator(
                e.operator_token.span(),
                e.operator_token.text.clone(),
                operand.get_type(),
            );
            operand
        }
    }

    fn bind_literal_expression(&self, e: &LiteralExpressionSyntax) -> Box<BoundExpression> {
        Box::new(BoundExpression::Literal(BoundLiteralExpression {
            value: e.value.clone(),
        }))
    }

    fn bind_parenthesized_expression(
        &mut self,
        e: &ParenthesizedExpressionSyntax,
    ) -> Box<BoundExpression> {
        self.bind_expression(e.expression.create_ref())
    }

    pub(crate) fn new(scope: Option<Box<BoundScope>>) -> Self {
        Self {
            diagnostics: DiagnosticBag::new(),
            scope: BoundScope::new(scope),
        }
    }

    fn bind_name_expression(&mut self, e: &NameExpressionSyntax) -> Box<BoundExpression> {
        let name = e.identifier_token.text.clone();

        let variable = self.scope.try_lookup(&name);

        match variable {
            Some(v) => Box::new(BoundExpression::Variable(BoundVariableExpression {
                variable: v.clone(),
            })),
            None => {
                self.diagnostics
                    .report_undefined_name(e.identifier_token.span(), &name);
                Box::new(BoundExpression::Literal(BoundLiteralExpression {
                    value: Object::Number(0),
                }))
            }
        }
    }

    fn bind_assignment_expression(
        &mut self,
        e: &AssignmentExpressionSyntax,
    ) -> Box<BoundExpression> {
        let name = e.identifier_token.text.clone();
        let expression = self.bind_expression(e.expression.create_ref());

        let variable = if let Some(variable) = self.scope.try_lookup(&name) {
            variable.clone()
        } else {
            self.diagnostics
                .report_undefined_name(e.identifier_token.span(), &name);
            return expression;
        };

        if variable.is_read_only {
            self.diagnostics
                .report_cannot_assign(e.equals_token.span(), &name);
        }

        if expression.get_type() != variable.kind {
            self.diagnostics.report_cannot_convert(
                SyntaxNodeRef::Expression(e.expression.create_ref()).span(),
                expression.get_type(),
                variable.kind,
            );
            return expression;
        }

        Box::new(BoundExpression::Assignment(BoundAssignmentExpression {
            variable,
            expression,
        }))
    }

    pub(crate) fn bind_global_scope(
        previous: Option<&BoundGlobalScope>,
        syntax: CompilationUnitSyntaxRef,
    ) -> BoundGlobalScope {
        let parent_scope = Self::create_parent_scopes(previous);
        let mut binder = Binder::new(parent_scope);
        let statement = binder.bind_statement(syntax.statement);
        let variables = binder
            .scope
            .get_declared_variables()
            .into_iter()
            .cloned()
            .collect();
        let diagnostics = binder.diagnostics.into_iter().collect::<Vec<_>>();
        BoundGlobalScope {
            previous: None,
            diagnostics,
            variables,
            statement: *statement,
        }
    }

    fn create_parent_scopes(mut previous: Option<&BoundGlobalScope>) -> Option<Box<BoundScope>> {
        let mut stack = Vec::new();
        while let Some(p) = previous {
            stack.push(p);
            previous = p.previous.as_ref().map(|p| p.as_ref());
        }

        let mut parent = None;
        while let Some(global) = stack.pop() {
            let mut scope = BoundScope::new(parent);
            for v in &global.variables {
                scope.try_declare(v.clone());
            }
            parent = Some(Box::new(scope));
        }
        parent
    }

    fn bind_block_statement(&mut self, s: &BlockStatementSyntax) -> Box<BoundStatement> {
        let mut statements = Vec::new();

        let mut scope = BoundScope::new(None);
        std::mem::swap(&mut scope, &mut self.scope);
        self.scope = BoundScope::new(Some(Box::new(scope)));

        for statement_syntax in &s.statements {
            let statement = self.bind_statement(statement_syntax.create_ref());
            statements.push(*statement);
        }

        let mut scope = BoundScope::new(None);
        std::mem::swap(&mut scope, self.scope.parent.as_mut().unwrap());
        self.scope = scope;

        Box::new(BoundStatement::Block(BoundBlockStatement { statements }))
    }

    fn bind_expression_statement(&mut self, s: &ExpressionStatementSyntax) -> Box<BoundStatement> {
        let expression = self.bind_expression(s.expression.create_ref());
        Box::new(BoundStatement::Expression(BoundExpressionStatement {
            expression: *expression,
        }))
    }

    fn bind_variable_declaration_statement(
        &mut self,
        s: &VariableDeclarationStatementSyntax,
    ) -> Box<BoundStatement> {
        let name = s.identifier.text.clone();
        let initializer = self.bind_expression(s.initializer.create_ref());
        let is_read_only = s.keyword.kind == SyntaxKind::LetKeyword;
        let variable = VariableSymbol {
            name: name.clone(),
            is_read_only,
            kind: initializer.get_type(),
        };
        if !self.scope.try_declare(variable.clone()) {
            self.diagnostics
                .report_variable_already_declared(s.identifier.span(), &name);
        }
        Box::new(BoundStatement::VariableDeclaration(
            BoundVariableDeclarationStatement {
                variable,
                initializer: *initializer,
            },
        ))
    }

    fn bind_if_statement(&mut self, s: &IfStatementSyntax) -> Box<BoundStatement> {
        let condition =
            self.bind_expression_with_type(s.condition.create_ref(), ObjectKind::Boolean);
        let then_statement = self.bind_statement(s.then_statement.create_ref());
        let else_statement = s
            .else_clause
            .as_ref()
            .map(|c| self.bind_statement(c.else_statement.create_ref()));
        Box::new(BoundStatement::If(BoundIfStatement {
            condition: *condition,
            then_statement,
            else_statement,
        }))
    }

    fn bind_while_statement(&mut self, s: &WhileStatementSyntax) -> Box<BoundStatement> {
        let condition =
            self.bind_expression_with_type(s.condition.create_ref(), ObjectKind::Boolean);
        let body = self.bind_statement(s.body.create_ref());
        Box::new(BoundStatement::While(BoundWhileStatement {
            condition: *condition,
            body,
        }))
    }
}
